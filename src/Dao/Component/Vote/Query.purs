{-|
Module: Dao.Component.Vote.Query
Description: Helpers for voting related contracts
-}
module Dao.Component.Vote.Query
  ( VoteInfo
  , mkAllVoteConstraintsAndLookups
  , spendFungibleUtxo
  , spendVoteNftUtxo
  , cancelVoteUtxo
  ) where

import Cardano.ToData (toData)
import Cardano.Types
  ( Asset(Asset)
  , AssetName
  , BigNum
  , PlutusScript
  , RedeemerDatum(RedeemerDatum)
  , ScriptHash
  , TransactionInput
  , TransactionOutput(TransactionOutput)
  )
import Cardano.Types.BigNum (fromBigInt, one, toBigInt) as BigNum
import Cardano.Types.Int (negate, one) as CTInt
import Cardano.Types.Value (add, singleton) as Value
import Contract.Address (PaymentPubKeyHash)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.Prelude
  ( type (/\)
  , bind
  , discard
  , mconcat
  , pure
  , traverse
  , unwrap
  , (#)
  , ($)
  , (*)
  , (+)
  , (/)
  , (/=)
  , (/\)
  , (<$>)
  , (<<<)
  )
import Contract.ScriptLookups as Lookups
import Contract.TxConstraints (InputWithScriptRef)
import Contract.TxConstraints as Constraints
import Contract.Value (CurrencySymbol, TokenName, Value, singleton, valueOf)
import Dao.Utils.Address (plutusAddressToPaymentPubKeyHash)
import Dao.Utils.Datum (extractOutputDatum)
import Dao.Utils.Query
  ( SpendPubKeyResult
  , UtxoInfo
  , findScriptUtxoBySymbolAndPkhInDatumAndProposalTokenNameInDatum
  , hasTokenWithNonAdaSymbol
  )
import Dao.Utils.Value (countOfTokenInValue, mkTokenName)
import Data.Array (catMaybes, filter, head)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing))
import JS.BigInt (BigInt, fromInt)
import LambdaBuffers.ApplicationTypes.Vote
  ( VoteActionRedeemer(VoteActionRedeemer'Count)
  , VoteDatum
  , VoteDirection
  , VoteMinterActionRedeemer(VoteMinterActionRedeemer'Burn)
  )
import Type.Proxy (Proxy(Proxy))

-- | Helper used by the 'countVote' contract.
-- | Make the constraints and lookups for all the vote UTXOs
-- | Also returns the 'VoteDirection' and amount for each vote encountered
mkAllVoteConstraintsAndLookups ::
  CurrencySymbol ->
  CurrencySymbol ->
  CurrencySymbol ->
  TokenName ->
  TokenName ->
  BigInt ->
  ScriptHash ->
  InputWithScriptRef ->
  InputWithScriptRef ->
  Map TransactionInput TransactionOutput ->
  Contract
    ( Array
        ( (VoteDirection /\ BigInt) /\ Lookups.ScriptLookups /\
            Constraints.TxConstraints
        )
    )
mkAllVoteConstraintsAndLookups
  voteNftSymbol
  voteSymbol
  fungibleSymbol
  proposalTokenName
  voteTokenName
  fungiblePercent
  votePolicyHash
  voteValidatorScriptRef
  votePolicyScriptRef
  utxos =
  catMaybes <$> traverse
    ( mkVoteUtxoConstraintsAndLookups
        voteNftSymbol
        voteSymbol
        fungibleSymbol
        proposalTokenName
        voteTokenName
        fungiblePercent
        votePolicyHash
        voteValidatorScriptRef
        votePolicyScriptRef
    )
    (Map.toUnfoldableUnordered utxos)

-- | Make the constraints and lookups for spending a particular vote UTXO
-- | Also calculate vote count for this vote, account for fungible tokens
-- | that act as a vote multiplier
mkVoteUtxoConstraintsAndLookups ::
  ScriptHash ->
  ScriptHash ->
  ScriptHash ->
  AssetName ->
  AssetName ->
  BigInt ->
  ScriptHash ->
  InputWithScriptRef ->
  InputWithScriptRef ->
  (TransactionInput /\ TransactionOutput) ->
  Contract
    ( Maybe
        ( (VoteDirection /\ BigInt) /\ Lookups.ScriptLookups /\
            Constraints.TxConstraints
        )
    )
mkVoteUtxoConstraintsAndLookups
  voteNftSymbol
  voteSymbol
  fungibleSymbol
  proposalTokenName
  voteTokenName
  fungiblePercent
  votePolicyHash
  voteValidatorScriptRef
  votePolicyScriptRef
  (txIn /\ txOut) =
  do
    logInfo' "Entering mkVoteUtxoConstraintsAndLookups"

    -- Extract the 'VoteDatum' fields
    voteDatum :: VoteDatum <- liftContractM "Failed to extract datum" $
      extractOutputDatum (Proxy :: Proxy VoteDatum) txOut

    let
      -- Extract the 'proposalTokenName' from the 'VoteDatum'
      -- This represents what proposal this vote was for
      -- The check below ensures that this is equal to the 'proposalTokenName'
      -- passed as an argument, otherwise the vote will not be counted
      voteProposalTokenName :: AssetName
      voteProposalTokenName = voteDatum # unwrap # _.proposalTokenName

    -- Only include votes for the specified proposal
    if (voteProposalTokenName /= proposalTokenName) then pure Nothing
    else do

      voteOwnerKey :: PaymentPubKeyHash <-
        liftContractM "Cannot get pkh" $ plutusAddressToPaymentPubKeyHash
          $ voteDatum
          # unwrap
          # _.voteOwner

      -- The vote 'pass' token name
      voteNftTokenName :: AssetName <-
        liftContractM "Could not make voteNft token name" $ mkTokenName
          "vote_pass"

      -- The vote 'multiplier' token name
      fungibleTokenName :: AssetName <-
        liftContractM "Could not make voteNft token name" $ mkTokenName
          "vote_fungible"

      let
        -- If the user holds fungible tokens we need to add the calculated weight
        -- of these tokens to the vote amount
        fungibleAmount = BigNum.toBigInt $ countOfToken fungibleSymbol txOut

      fungibleAmountBigNum <-
        liftContractM "Could not convert fungibleAmount to BigNum" $
          BigNum.fromBigInt fungibleAmount

      let
        fungibleToken :: Value
        fungibleToken =
          Value.singleton fungibleSymbol fungibleTokenName
            fungibleAmountBigNum

        voteNftToken :: Value
        voteNftToken = Value.singleton voteNftSymbol voteNftTokenName BigNum.one

      voteOwnerValue <- liftContractM "Could not build voteOwnerValue" $
        Value.add voteNftToken fungibleToken

      let
        fungibleVoteWeight = (fungibleAmount * fungiblePercent) / (fromInt 1000)

        voteDirection' :: VoteDirection
        voteDirection' = voteDatum # unwrap # _.direction

        voteAmount :: BigInt
        voteAmount = (fromInt 1) + fungibleVoteWeight

        burnVoteRedeemer :: RedeemerDatum
        burnVoteRedeemer = RedeemerDatum $ toData VoteMinterActionRedeemer'Burn

        lookups' :: Lookups.ScriptLookups
        lookups' = -- mempty

          mconcat
            [ Lookups.unspentOutputs $ Map.singleton txIn txOut
            ]

        constraints' :: Constraints.TxConstraints
        constraints' = mconcat
          [ Constraints.mustSpendScriptOutputUsingScriptRef txIn
              (RedeemerDatum $ toData VoteActionRedeemer'Count)
              voteValidatorScriptRef
          , Constraints.mustPayToPubKey voteOwnerKey voteOwnerValue
          -- ^ Return the 'voteNft', and 'fungibleToken(s)' if any
          , Constraints.mustMintCurrencyWithRedeemerUsingScriptRef
              votePolicyHash
              burnVoteRedeemer
              voteTokenName
              (CTInt.negate CTInt.one)
              votePolicyScriptRef
          ]

      pure $ Just ((voteDirection' /\ voteAmount) /\ lookups' /\ constraints')
  where
  countOfToken :: ScriptHash -> TransactionOutput -> BigNum
  countOfToken symbol txOut = countOfTokenInValue symbol (unwrap txOut).amount

type VoteInfo = UtxoInfo VoteDatum

-- | Spend the vote UTXO corresponding to the user's PKH
-- | Ensure it is owned by the user and was a vote on
-- | the provided proposal (proposalTokenName is checked for this)
cancelVoteUtxo ::
  VoteActionRedeemer ->
  ScriptHash ->
  PaymentPubKeyHash ->
  AssetName ->
  PlutusScript ->
  Contract VoteInfo
cancelVoteUtxo voteActionRedeemer symbol userPkh proposalTokenName voteValidator =
  do
    logInfo' "Entering cancelVoteUtxo contract"
    findScriptUtxoBySymbolAndPkhInDatumAndProposalTokenNameInDatum
      (RedeemerDatum $ toData voteActionRedeemer)
      symbol
      userPkh
      proposalTokenName
      voteValidator

-- | Spend vote pass ('voteNft') UTXO
spendVoteNftUtxo ::
  ScriptHash ->
  Map TransactionInput TransactionOutput ->
  Contract SpendPubKeyResult
spendVoteNftUtxo voteNftSymbol utxos = do
  logInfo' "Entering spendVoteNftUtxo contract"

  (txIn /\ txOut'@(TransactionOutput txOut)) <-
    liftContractM
      "User does not hold a voteNft token (votePass) so is ineligble to vote"
      (filterOneOfTokenInUtxo voteNftSymbol utxos)

  -- The vote 'pass' token name
  voteNftTokenName :: TokenName <-
    liftContractM "Could not make voteNft token name" $ mkTokenName
      "vote_pass"

  let
    lookups :: Lookups.ScriptLookups
    lookups = mconcat [ Lookups.unspentOutputs $ Map.singleton txIn txOut' ]

    constraints :: Constraints.TxConstraints
    constraints = mconcat [ Constraints.mustSpendPubKeyOutput txIn ]

    value :: Value
    value = txOut.amount

    voteNftValue :: Value
    voteNftValue =
      singleton voteNftSymbol voteNftTokenName $
        valueOf (Asset voteNftSymbol voteNftTokenName) value

  pure { lookups, constraints, value: voteNftValue }

-- | Spend fungible vote multiplier UTXO
spendFungibleUtxo ::
  CurrencySymbol ->
  CurrencySymbol ->
  TokenName ->
  Map TransactionInput TransactionOutput ->
  Contract (Maybe SpendPubKeyResult)
spendFungibleUtxo fungibleSymbol voteNftSymbol fungibleTokenName utxos = do
  logInfo' "Entering spendFungibleUtxo contract"

  case filterOneOfTokenInUtxo fungibleSymbol utxos of
    Nothing -> pure Nothing
    Just (txIn /\ txOutFungible@(TransactionOutput txOut)) -> do
      let
        lookups :: Lookups.ScriptLookups
        lookups = mconcat
          [ Lookups.unspentOutputs $ Map.singleton txIn txOutFungible ]

        constraints :: Constraints.TxConstraints
        constraints = mconcat [ Constraints.mustSpendPubKeyOutput txIn ]

        value :: Value
        value = txOut.amount

        fungibleValue :: Value
        fungibleValue =
          singleton fungibleSymbol fungibleTokenName $
            valueOf (Asset fungibleSymbol fungibleTokenName) value

      pure $ Just { lookups, constraints, value: fungibleValue }

filterOneOfTokenInUtxo ::
  CurrencySymbol ->
  Map TransactionInput TransactionOutput ->
  Maybe (TransactionInput /\ TransactionOutput)
filterOneOfTokenInUtxo symbol = head
  <<< filter (hasTokenWithNonAdaSymbol symbol)
  <<<
    Map.toUnfoldable

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

import Contract.Address (PaymentPubKeyHash)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData
  ( Redeemer(Redeemer)
  , toData
  )
import Contract.Prelude
  ( type (/\)
  , bind
  , discard
  , mconcat
  , negate
  , one
  , pure
  , traverse
  , unwrap
  , (#)
  , ($)
  , (*)
  , (+)
  , (/)
  , (/\)
  , (<<<)
  , (<>)
  , (<$>)
  , (/=)
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicyHash, Validator)
import Contract.Transaction
  ( TransactionInput
  , TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  )
import Contract.TxConstraints (InputWithScriptRef(SpendInput, RefInput))
import Contract.TxConstraints as Constraints
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , Value
  , singleton
  , valueOf
  )
import Dao.Utils.Address (addressToPaymentPubKeyHash)
import Dao.Utils.Datum (extractOutputDatum)
import Dao.Utils.Query
  ( SpendPubKeyResult
  , UtxoInfo
  , findScriptUtxoBySymbolAndPkhInDatumAndProposalTokenNameInDatum
  , hasTokenWithSymbol
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
  MintingPolicyHash ->
  InputWithScriptRef ->
  InputWithScriptRef ->
  Map TransactionInput TransactionOutputWithRefScript ->
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
  CurrencySymbol ->
  CurrencySymbol ->
  CurrencySymbol ->
  TokenName ->
  TokenName ->
  BigInt ->
  MintingPolicyHash ->
  InputWithScriptRef ->
  InputWithScriptRef ->
  (TransactionInput /\ TransactionOutputWithRefScript) ->
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
      voteProposalTokenName :: TokenName
      voteProposalTokenName = voteDatum # unwrap # _.proposalTokenName

    -- Only include votes for the specified proposal
    if (voteProposalTokenName /= proposalTokenName) then pure Nothing else do

      voteOwnerKey :: PaymentPubKeyHash <-
        liftContractM "Cannot get pkh" $ addressToPaymentPubKeyHash $ voteDatum
          # unwrap
          # _.voteOwner

      -- The vote 'pass' token name
      voteNftTokenName :: TokenName <-
        liftContractM "Could not make voteNft token name" $ mkTokenName
          "vote_pass"

      -- The vote 'multiplier' token name
      fungibleTokenName :: TokenName <-
        liftContractM "Could not make voteNft token name" $ mkTokenName
          "vote_fungible"

      let
        -- If the user holds fungible tokens we need to add the calculated weight
        -- of these tokens to the vote amount
        fungibleAmount = countOfToken fungibleSymbol txOut
        fungibleVoteWeight = (fungibleAmount * fungiblePercent) / (fromInt 1000)

        voteDirection' :: VoteDirection
        voteDirection' = voteDatum # unwrap # _.direction

        voteAmount :: BigInt
        voteAmount = (fromInt 1) + fungibleVoteWeight

        voteNftToken :: Value
        voteNftToken = singleton voteNftSymbol voteNftTokenName one

        fungibleToken :: Value
        fungibleToken = singleton fungibleSymbol fungibleTokenName fungibleAmount

        burnVoteRedeemer :: Redeemer
        burnVoteRedeemer = Redeemer $ toData VoteMinterActionRedeemer'Burn

        lookups' :: Lookups.ScriptLookups
        lookups' = -- mempty
          mconcat
            [ Lookups.unspentOutputs $ Map.singleton txIn txOut
            ]

        constraints' :: Constraints.TxConstraints
        constraints' = mconcat
          [ Constraints.mustSpendScriptOutputUsingScriptRef txIn
              (Redeemer $ toData VoteActionRedeemer'Count)
              voteValidatorScriptRef
          , Constraints.mustPayToPubKey voteOwnerKey
              (voteNftToken <> fungibleToken)
          -- ^ Return the 'voteNft', and 'fungibleToken(s)' if any
          , Constraints.mustMintCurrencyWithRedeemerUsingScriptRef
              votePolicyHash
              burnVoteRedeemer
              voteTokenName
              (negate one)
              votePolicyScriptRef
          ]

      pure $ Just ((voteDirection' /\ voteAmount) /\ lookups' /\ constraints')
  where
  countOfToken :: CurrencySymbol -> TransactionOutputWithRefScript -> BigInt
  countOfToken symbol txOut = countOfTokenInValue symbol value
    where
    value = txOut # unwrap # _.output # unwrap # _.amount

type VoteInfo = UtxoInfo VoteDatum

-- | Spend the vote UTXO corresponding to the user's PKH
-- | Ensure it is owned by the user and was a vote on
-- | the provided proposal (proposalTokenName is checked for this)
cancelVoteUtxo ::
  VoteActionRedeemer ->
  CurrencySymbol ->
  PaymentPubKeyHash ->
  TokenName ->
  Validator ->
  Contract VoteInfo
cancelVoteUtxo voteActionRedeemer symbol userPkh proposalTokenName voteValidator =
  do
    logInfo' "Entering cancelVoteUtxo contract"
    findScriptUtxoBySymbolAndPkhInDatumAndProposalTokenNameInDatum
      (Redeemer $ toData voteActionRedeemer)
      symbol
      userPkh
      proposalTokenName
      voteValidator

-- | Spend vote pass ('voteNft') UTXO
spendVoteNftUtxo ::
  CurrencySymbol ->
  Map TransactionInput TransactionOutputWithRefScript ->
  Contract SpendPubKeyResult
spendVoteNftUtxo voteNftSymbol utxos = do
  logInfo' "Entering spendVoteNftUtxo contract"

  (txIn /\ txOut'@(TransactionOutputWithRefScript txOut)) <-
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
    value = txOut.output # unwrap # _.amount

    voteNftValue :: Value
    voteNftValue = singleton voteNftSymbol voteNftTokenName
      (valueOf value voteNftSymbol voteNftTokenName)

  pure { lookups, constraints, value: voteNftValue }

-- | Spend fungible vote multiplier UTXO
spendFungibleUtxo ::
  CurrencySymbol ->
  CurrencySymbol ->
  TokenName ->
  Map TransactionInput TransactionOutputWithRefScript ->
  Contract (Maybe SpendPubKeyResult)
spendFungibleUtxo fungibleSymbol voteNftSymbol fungibleTokenName utxos = do
  logInfo' "Entering spendFungibleUtxo contract"

  case filterOneOfTokenInUtxo fungibleSymbol utxos of
    Nothing -> pure Nothing
    Just (txIn /\ txOutFungible@(TransactionOutputWithRefScript txOut)) -> do
      let
        lookups :: Lookups.ScriptLookups
        lookups = mconcat
          [ Lookups.unspentOutputs $ Map.singleton txIn txOutFungible ]

        constraints :: Constraints.TxConstraints
        constraints = mconcat [ Constraints.mustSpendPubKeyOutput txIn ]

        value :: Value
        value = txOut.output # unwrap # _.amount

        fungibleValue :: Value
        fungibleValue = singleton fungibleSymbol fungibleTokenName
          (valueOf value fungibleSymbol fungibleTokenName)

      pure $ Just { lookups, constraints, value: fungibleValue }

filterOneOfTokenInUtxo ::
  CurrencySymbol ->
  Map TransactionInput TransactionOutputWithRefScript ->
  Maybe (TransactionInput /\ TransactionOutputWithRefScript)
filterOneOfTokenInUtxo symbol = head <<< filter (hasTokenWithSymbol symbol) <<<
  Map.toUnfoldable

inputWithScriptRefToUnspentOutputs ::
  InputWithScriptRef ->
  Map.Map TransactionInput TransactionOutputWithRefScript
inputWithScriptRefToUnspentOutputs ref =
  case ref of
    SpendInput inp -> Map.singleton (unwrap inp).input (unwrap inp).output
    RefInput inp -> Map.singleton (unwrap inp).input (unwrap inp).output

{-|
Module: Dao.Component.Vote.Query
Description: Helpers for voting related contracts
-}
module Dao.Component.Vote.Query
  ( VoteInfo
  , mkAllVoteConstraintsAndLookups
  , spendFungibleUtxo
  , spendVoteNftUtxo
  , spendFungibleUtxo
  , cancelVoteUtxo
  ) where

import Contract.Address (PaymentPubKeyHash)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData
  ( Datum(Datum)
  , OutputDatum(OutputDatum)
  , Redeemer(Redeemer)
  , fromData
  , toData
  , unitRedeemer
  )
import Contract.Prelude
  ( type (/\)
  , any
  , bind
  , discard
  , mconcat
  , mempty
  , negate
  , one
  , pure
  , show
  , traverse
  , unwrap
  , wrap
  , (#)
  , ($)
  , (&&)
  , (*)
  , (+)
  , (/)
  , (/\)
  , (<<<)
  , (<>)
  , (==)
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy, Validator)
import Contract.Transaction
  ( TransactionInput
  , TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  )
import Contract.TxConstraints as Constraints
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , Value
  , getValue
  , singleton
  , symbols
  , valueOf
  )
import Ctl.Internal.Plutus.Types.AssocMap
  ( Map(Map)
  , lookup
  ) as Plutus.Map
import Dao.Utils.Address (addressToPaymentPubKeyHash)
import Dao.Utils.Datum (extractOutputDatum)
import Dao.Utils.Error (guardContract)
import Dao.Utils.Query
  ( QueryType(Reference, Spend)
  , SpendPubKeyResult
  , UtxoInfo
  , findScriptUtxoBySymbol
  , findScriptUtxoBySymbolAndPkhInDatumAndProposalTokenNameInDatum
  , hasTokenWithSymbol
  )
import Dao.Utils.Value (countOfTokenInValue, mkTokenName)
import Data.Array (filter, head)
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
  utxos =
  traverse
    ( mkVoteUtxoConstraintsAndLookups
        voteNftSymbol
        voteSymbol
        fungibleSymbol
        proposalTokenName
        voteTokenName
        fungiblePercent
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
  (TransactionInput /\ TransactionOutputWithRefScript) ->
  Contract
    ( (VoteDirection /\ BigInt) /\ Lookups.ScriptLookups /\
        Constraints.TxConstraints
    )
mkVoteUtxoConstraintsAndLookups
  voteNftSymbol
  voteSymbol
  fungibleSymbol
  proposalTokenName
  voteTokenName
  fungiblePercent
  (txIn /\ txOut) =
  do
    logInfo' "Entering mkVoteUtxoConstraintsAndLookups"

    logInfo' $ "fungibleSymbol in mkAllVoteConstraintsAndLookups: " <> show
      fungibleSymbol
    logInfo' $ "txOut in mkAllVoteConstraintsAndLookups: " <> show txOut

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

    guardContract "Vote is for another proposal"
      $ voteProposalTokenName
      == proposalTokenName

    voteValue :: Value <-
      liftContractM "Vote value does not contain both vote NFT and vote token" $
        extractToken voteNftSymbol voteSymbol txOut

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

      burnVoteValue :: Value
      burnVoteValue = singleton voteSymbol voteTokenName (negate one)

      burnVoteRedeemer :: Redeemer
      burnVoteRedeemer = Redeemer $ toData VoteMinterActionRedeemer'Burn

      lookups' :: Lookups.ScriptLookups
      lookups' = mconcat
        [ Lookups.unspentOutputs $
            Map.singleton txIn txOut
        ]

      constraints' :: Constraints.TxConstraints
      constraints' = mconcat
        [ Constraints.mustSpendScriptOutput txIn
            (Redeemer $ toData VoteActionRedeemer'Count)
        , Constraints.mustPayToPubKey voteOwnerKey
            (voteNftToken <> fungibleToken)
        -- ^ Return the 'voteNft', and 'fungibleToken(s)' if any
        , Constraints.mustMintValueWithRedeemer burnVoteRedeemer burnVoteValue
        ]

    pure ((voteDirection' /\ voteAmount) /\ lookups' /\ constraints')
  where
  countOfToken :: CurrencySymbol -> TransactionOutputWithRefScript -> BigInt
  countOfToken symbol txOut = countOfTokenInValue symbol value
    where
    value = txOut # unwrap # _.output # unwrap # _.amount

  extractToken ::
    CurrencySymbol ->
    CurrencySymbol ->
    TransactionOutputWithRefScript ->
    Maybe Value
  extractToken voteSymbol voteNftSymbol txOut =
    let
      value = txOut # unwrap # _.output # unwrap # _.amount
    in
      if
        (any (_ == voteSymbol) $ symbols value) &&
          (any (_ == voteNftSymbol) $ symbols value) then Just value
      else Nothing

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

  logInfo' $ "txOut in spendVoteNftUtxo: " <> show txOut

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

  logInfo' $ "value in voteNft: " <> show voteNftValue

  pure { lookups, constraints, value: voteNftValue }

-- | Spend fungible vote multiplier UTXO
spendFungibleUtxo ::
  CurrencySymbol ->
  CurrencySymbol ->
  TokenName ->
  Map TransactionInput TransactionOutputWithRefScript ->
  Contract SpendPubKeyResult
spendFungibleUtxo fungibleSymbol voteNftSymbol fungibleTokenName utxos = do
  logInfo' "Entering spendVoteNftUtxo contract"

  (txIn /\ txOutFungible@(TransactionOutputWithRefScript txOut)) <-
    liftContractM
      "User does not hold any fungible tokens"
      (filterOneOfTokenInUtxo fungibleSymbol utxos)

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

  logInfo' $ "value in fungible: " <> show value
  logInfo' $ "fungibleValue in fungible: " <> show fungibleValue

  pure { lookups, constraints, value: fungibleValue }

filterOneOfTokenInUtxo ::
  CurrencySymbol ->
  Map TransactionInput TransactionOutputWithRefScript ->
  Maybe (TransactionInput /\ TransactionOutputWithRefScript)
filterOneOfTokenInUtxo symbol = head <<< filter (hasTokenWithSymbol symbol) <<<
  Map.toUnfoldable

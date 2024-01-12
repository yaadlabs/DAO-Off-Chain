module Dao.Component.Vote.Query
  ( VoteInfo
  , referenceVoteUtxo
  , spendVoteUtxo
  , spendVoteNftUtxo
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
  , negate
  , one
  , pure
  , unwrap
  , (#)
  , ($)
  , (&&)
  , (/\)
  , (==)
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy, Validator)
import Contract.Transaction
  ( TransactionInput
  , TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  )
import Contract.TxConstraints as Constraints
import Contract.Value (CurrencySymbol, TokenName, Value, singleton, symbols)
import Dao.Utils.Address (addressToPaymentPubKeyHash)
import Dao.Utils.Query
  ( QueryType(Reference, Spend)
  , UtxoInfo
  , findKeyUtxoBySymbol
  , findScriptUtxoBySymbol
  )
import Dao.Utils.Query (findKeyUtxoBySymbol)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing))
import LambdaBuffers.ApplicationTypes.Vote
  ( VoteActionRedeemer(VoteActionRedeemer'Count)
  , VoteDatum
  , VoteDirection
  )
import Type.Proxy (Proxy(Proxy))

type VoteResult =
  { voteDirection :: VoteDirection
  , lookups :: Lookups.ScriptLookups
  , constraints :: Constraints.TxConstraints
  }

mkVoteUtxoConstraintsAndLookups ::
  CurrencySymbol ->
  CurrencySymbol ->
  TokenName ->
  TokenName ->
  MintingPolicy ->
  (TransactionInput /\ TransactionOutputWithRefScript) ->
  Contract VoteResult
mkVoteUtxoConstraintsAndLookups
  voteNftSymbol
  voteSymbol
  voteNftTokenName
  voteTokenName
  votePolicyScript
  (txIn /\ txOut) =
  do
    logInfo' "Entering mkVoteUtxoConstraintsAndLookups"

    voteDatum :: VoteDatum <- liftContractM "Failed to extract datum" $
      extractOutputDatum
        txOut
    voteValue :: Value <-
      liftContractM "Vote value does not contain both vote NFT and vote token" $
        extractToken voteNftSymbol voteSymbol txOut

    voteOwnerKey :: PaymentPubKeyHash <-
      liftContractM "Cannot get pkh" $ addressToPaymentPubKeyHash $ voteDatum
        # unwrap
        # _.voteOwner

    let
      voteDirection' :: VoteDirection
      voteDirection' = voteDatum # unwrap # _.direction

      voteNftToken :: Value
      voteNftToken = singleton voteNftSymbol voteNftTokenName one

      burnVoteValue :: Value
      burnVoteValue = singleton voteSymbol voteTokenName (negate one)

      lookups' :: Lookups.ScriptLookups
      lookups' = mconcat
        [ Lookups.unspentOutputs $
            Map.singleton txIn txOut
        , Lookups.mintingPolicy votePolicyScript
        ]

      constraints' :: Constraints.TxConstraints
      constraints' = mconcat
        [ Constraints.mustSpendScriptOutput txIn
            (Redeemer $ toData $ VoteActionRedeemer'Count)
        , Constraints.mustPayToPubKey voteOwnerKey voteNftToken
        , Constraints.mustMintValue burnVoteValue
        ]

    pure
      { voteDirection: voteDirection'
      , lookups: lookups'
      , constraints: constraints'
      }
  where
  extractOutputDatum :: TransactionOutputWithRefScript -> Maybe VoteDatum
  extractOutputDatum (TransactionOutputWithRefScript txOut) =
    case txOut.output # unwrap # _.datum of
      OutputDatum (Datum rawInlineDatum) -> case fromData rawInlineDatum of
        Just (datum :: VoteDatum) -> Just datum
        _ -> Nothing
      _ -> Nothing

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

referenceVoteUtxo ::
  CurrencySymbol ->
  Validator ->
  Contract VoteInfo
referenceVoteUtxo voteSymbol voteValidator = do
  logInfo' "Entering referenceVoteUtxo contract"
  findScriptUtxoBySymbol
    (Proxy :: Proxy VoteDatum)
    Reference
    unitRedeemer
    voteSymbol
    voteValidator

spendVoteUtxo ::
  VoteActionRedeemer ->
  CurrencySymbol ->
  Validator ->
  Contract VoteInfo
spendVoteUtxo voteActionRedeemer voteSymbol voteValidator = do
  logInfo' "Entering spendVoteUtxo contract"
  findScriptUtxoBySymbol
    (Proxy :: Proxy VoteDatum)
    Spend
    (Redeemer $ toData $ voteActionRedeemer)
    voteSymbol
    voteValidator

spendVoteNftUtxo ::
  CurrencySymbol ->
  Map TransactionInput TransactionOutputWithRefScript ->
  Contract Value
spendVoteNftUtxo voteNftSymbol utxoMap = do
  logInfo' "Entering spendVoteNftUtxo contract"
  findKeyUtxoBySymbol voteNftSymbol utxoMap

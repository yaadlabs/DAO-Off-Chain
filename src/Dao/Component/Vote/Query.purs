module Dao.Component.Vote.Query
  ( VoteInfo
  , referenceVoteUtxo
  , spendVoteUtxo
  , spendVoteNftUtxo
  ) where

import Contract.Log (logInfo')
import Contract.Monad (Contract)
import Contract.PlutusData (Redeemer(Redeemer), toData, unitRedeemer)
import Contract.Prelude (discard, ($))
import Contract.Scripts (Validator)
import Contract.Transaction (TransactionInput, TransactionOutputWithRefScript)
import Contract.Value (CurrencySymbol)
import Contract.Value (CurrencySymbol, Value)
import Dao.Utils.Query
  ( QueryType(Reference, Spend)
  , UtxoInfo
  , findKeyUtxoBySymbol
  , findScriptUtxoBySymbol
  )
import Dao.Utils.Query (findKeyUtxoBySymbol)
import Data.Map (Map)
import LambdaBuffers.ApplicationTypes.Vote (VoteActionRedeemer, VoteDatum)
import Type.Proxy (Proxy(Proxy))

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

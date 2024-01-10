module Dao.Component.Vote.Query
  ( VoteInfo
  , referenceVoteUtxo
  , spendVoteUtxo
  ) where

import Contract.Log (logInfo')
import Contract.Monad (Contract)
import Contract.PlutusData (Redeemer(Redeemer), toData, unitRedeemer)
import Contract.Prelude (discard, ($))
import Contract.Scripts (Validator)
import Contract.Value (CurrencySymbol)
import Dao.Utils.Query (QueryType(Reference, Spend), UtxoInfo, findUtxoBySymbol)
import LambdaBuffers.ApplicationTypes.Vote (VoteActionRedeemer, VoteDatum)
import Type.Proxy (Proxy(Proxy))

type VoteInfo = UtxoInfo VoteDatum

referenceVoteUtxo ::
  CurrencySymbol ->
  Validator ->
  Contract VoteInfo
referenceVoteUtxo voteSymbol voteValidator = do
  logInfo' "Entering referenceVoteUtxo contract"
  findUtxoBySymbol
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
  findUtxoBySymbol
    (Proxy :: Proxy VoteDatum)
    Spend
    (Redeemer $ toData $ voteActionRedeemer)
    voteSymbol
    voteValidator

module Dao.Component.Index.Query
  ( IndexInfo
  , referenceIndexUtxo
  , spendIndexUtxo
  ) where

import Contract.Log (logInfo')
import Contract.Monad (Contract)
import Contract.Prelude (discard)
import Contract.Scripts (Validator)
import Contract.Value (CurrencySymbol)
import Dao.Utils.Query (QueryType(Reference, Spend), UtxoInfo, findUtxoBySymbol)
import LambdaBuffers.ApplicationTypes.Index (IndexNftDatum)
import Type.Proxy (Proxy(Proxy))

type IndexInfo = UtxoInfo IndexNftDatum

referenceIndexUtxo ::
  CurrencySymbol ->
  Validator ->
  Contract IndexInfo
referenceIndexUtxo indexSymbol indexValidator = do
  logInfo' "Entering referenceIndexUtxo contract"
  findUtxoBySymbol (Proxy :: Proxy IndexNftDatum) Reference
    indexSymbol
    indexValidator

spendIndexUtxo ::
  CurrencySymbol ->
  Validator ->
  Contract IndexInfo
spendIndexUtxo indexSymbol indexValidator = do
  logInfo' "Entering spendIndexUtxo contract"
  findUtxoBySymbol (Proxy :: Proxy IndexNftDatum) Spend
    indexSymbol
    indexValidator

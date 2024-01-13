module Dao.Component.Index.Query
  ( IndexInfo
  , referenceIndexUtxo
  , spendIndexUtxo
  ) where

import Contract.Log (logInfo')
import Contract.Monad (Contract)
import Contract.PlutusData (unitRedeemer)
import Contract.Prelude (discard)
import Contract.Scripts (Validator)
import Contract.Value (CurrencySymbol)
import Dao.Utils.Query
  ( QueryType(Reference, Spend)
  , UtxoInfo
  , findScriptUtxoBySymbol
  )
import LambdaBuffers.ApplicationTypes.Index (IndexNftDatum)
import Type.Proxy (Proxy(Proxy))

type IndexInfo = UtxoInfo IndexNftDatum

referenceIndexUtxo ::
  CurrencySymbol ->
  Validator ->
  Contract IndexInfo
referenceIndexUtxo indexSymbol indexValidator = do
  logInfo' "Entering referenceIndexUtxo contract"
  findScriptUtxoBySymbol
    (Proxy :: Proxy IndexNftDatum)
    Reference
    unitRedeemer
    indexSymbol
    indexValidator

spendIndexUtxo ::
  CurrencySymbol ->
  Validator ->
  Contract IndexInfo
spendIndexUtxo indexSymbol indexValidator = do
  logInfo' "Entering spendIndexUtxo contract"
  findScriptUtxoBySymbol
    (Proxy :: Proxy IndexNftDatum)
    Spend
    unitRedeemer
    indexSymbol
    indexValidator

module Dao.Component.Config.Query
  ( ConfigInfo
  , referenceConfigUtxo
  , spendConfigUtxo
  ) where

import Contract.Log (logInfo')
import Contract.Monad (Contract)
import Contract.PlutusData (unitRedeemer)
import Contract.Prelude (discard)
import Contract.Scripts (Validator)
import Contract.Value (CurrencySymbol)
import Dao.Utils.Query (QueryType(Reference, Spend), UtxoInfo, findUtxoBySymbol)
import LambdaBuffers.ApplicationTypes.Configuration (DynamicConfigDatum)
import Type.Proxy (Proxy(Proxy))

type ConfigInfo = UtxoInfo DynamicConfigDatum

referenceConfigUtxo ::
  CurrencySymbol ->
  Validator ->
  Contract ConfigInfo
referenceConfigUtxo configSymbol configValidator = do
  logInfo' "Entering referenceConfigUtxo contract"
  findUtxoBySymbol
    (Proxy :: Proxy DynamicConfigDatum)
    Reference
    unitRedeemer
    configSymbol
    configValidator

spendConfigUtxo ::
  CurrencySymbol ->
  Validator ->
  Contract ConfigInfo
spendConfigUtxo configSymbol configValidator = do
  logInfo' "Entering spendConfigUtxo contract"
  findUtxoBySymbol
    (Proxy :: Proxy DynamicConfigDatum)
    Spend
    unitRedeemer
    configSymbol
    configValidator

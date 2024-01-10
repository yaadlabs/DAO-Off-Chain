module Dao.Component.Tally.Query
  ( TallyInfo
  , referenceTallyUtxo
  , spendTallyUtxo
  ) where

import Contract.Log (logInfo')
import Contract.Monad (Contract)
import Contract.PlutusData (unitRedeemer)
import Contract.Prelude (discard)
import Contract.Scripts (Validator)
import Contract.Value (CurrencySymbol)
import Dao.Utils.Query (QueryType(Reference, Spend), UtxoInfo, findUtxoBySymbol)
import LambdaBuffers.ApplicationTypes.Tally (TallyStateDatum)
import Type.Proxy (Proxy(Proxy))

type TallyInfo = UtxoInfo TallyStateDatum

referenceTallyUtxo ::
  CurrencySymbol ->
  Validator ->
  Contract TallyInfo
referenceTallyUtxo tallySymbol tallyValidator = do
  logInfo' "Entering referenceTallyUtxo contract"
  findUtxoBySymbol
    (Proxy :: Proxy TallyStateDatum)
    Reference
    unitRedeemer
    tallySymbol
    tallyValidator

spendTallyUtxo ::
  CurrencySymbol ->
  Validator ->
  Contract TallyInfo
spendTallyUtxo tallySymbol tallyValidator = do
  logInfo' "Entering spendTallyUtxo contract"
  findUtxoBySymbol
    (Proxy :: Proxy TallyStateDatum)
    Spend
    unitRedeemer
    tallySymbol
    tallyValidator

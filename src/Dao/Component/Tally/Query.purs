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
import Contract.Value (CurrencySymbol, TokenName)
import Dao.Utils.Query
  ( QueryType(Reference, Spend)
  , UtxoInfo
  , findScriptUtxoBySymbol
  , findScriptUtxoByToken
  )
import LambdaBuffers.ApplicationTypes.Tally (TallyStateDatum)
import Type.Proxy (Proxy(Proxy))

type TallyInfo = UtxoInfo TallyStateDatum

referenceTallyUtxo ::
  CurrencySymbol ->
  Validator ->
  Contract TallyInfo
referenceTallyUtxo tallySymbol tallyValidator = do
  logInfo' "Entering referenceTallyUtxo contract"
  findScriptUtxoBySymbol
    (Proxy :: Proxy TallyStateDatum)
    Reference
    unitRedeemer
    tallySymbol
    tallyValidator

spendTallyUtxo ::
  CurrencySymbol ->
  TokenName ->
  Validator ->
  Contract TallyInfo
spendTallyUtxo tallySymbol proposalTokenName tallyValidator = do
  logInfo' "Entering spendTallyUtxo contract"
  findScriptUtxoByToken
    (Proxy :: Proxy TallyStateDatum)
    Spend
    unitRedeemer
    tallySymbol
    proposalTokenName
    tallyValidator

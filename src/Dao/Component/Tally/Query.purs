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
import Contract.TxConstraints (InputWithScriptRef)
import Contract.Value (CurrencySymbol, TokenName)
import Dao.Utils.Query
  ( QueryType(Reference, Spend)
  , UtxoInfo
  , findScriptUtxoByToken
  , findScriptUtxoByTokenWithScriptRef
  )
import LambdaBuffers.ApplicationTypes.Tally (TallyStateDatum)
import Type.Proxy (Proxy(Proxy))

type TallyInfo = UtxoInfo TallyStateDatum

referenceTallyUtxo ::
  CurrencySymbol ->
  TokenName ->
  Validator ->
  Contract TallyInfo
referenceTallyUtxo tallySymbol proposalTokenName tallyValidator = do
  logInfo' "Entering referenceTallyUtxo contract"
  findScriptUtxoByToken
    (Proxy :: Proxy TallyStateDatum)
    Reference
    unitRedeemer
    tallySymbol
    proposalTokenName
    tallyValidator

spendTallyUtxo ::
  CurrencySymbol ->
  TokenName ->
  Validator ->
  InputWithScriptRef ->
  Contract TallyInfo
spendTallyUtxo tallySymbol proposalTokenName tallyValidator scriptRef = do
  logInfo' "Entering spendTallyUtxo contract"
  findScriptUtxoByTokenWithScriptRef
    (Proxy :: Proxy TallyStateDatum)
    Spend
    unitRedeemer
    tallySymbol
    proposalTokenName
    tallyValidator
    scriptRef

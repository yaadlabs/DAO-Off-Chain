module Dao.Component.Treasury.Query
  ( TreasuryInfo
  , referenceTreasuryUtxo
  , spendTreasuryUtxo
  ) where

import Contract.Log (logInfo')
import Contract.Monad (Contract)
import Contract.PlutusData (Datum, Redeemer(Redeemer), toData, unitRedeemer)
import Contract.Prelude (discard, ($))
import Contract.Scripts (Validator)
import Contract.Value (CurrencySymbol)
import Dao.Utils.Query
  ( QueryType(Reference, Spend)
  , UtxoInfo
  , findScriptUtxoBySymbol
  )
import LambdaBuffers.ApplicationTypes.Proposal (ProposalType)
import Type.Proxy (Proxy(Proxy))

type TreasuryInfo = UtxoInfo Datum

referenceTreasuryUtxo ::
  CurrencySymbol ->
  Validator ->
  Contract TreasuryInfo
referenceTreasuryUtxo treasurySymbol treasuryValidator = do
  logInfo' "Entering referenceTreasuryUtxo contract"
  findScriptUtxoBySymbol
    (Proxy :: Proxy Datum)
    Reference
    unitRedeemer
    treasurySymbol
    treasuryValidator

spendTreasuryUtxo ::
  CurrencySymbol ->
  Validator ->
  Contract TreasuryInfo
spendTreasuryUtxo treasurySymbol treasuryValidator = do
  logInfo' "Entering spendTreasuryUtxo contract"
  findScriptUtxoBySymbol
    (Proxy :: Proxy Datum)
    Spend
    unitRedeemer
    treasurySymbol
    treasuryValidator

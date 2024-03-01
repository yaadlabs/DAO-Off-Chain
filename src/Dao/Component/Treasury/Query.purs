module Dao.Component.Treasury.Query
  ( TreasuryInfo
  , spendTreasuryUtxo
  ) where

import Contract.Log (logInfo')
import Contract.Monad (Contract)
import Contract.PlutusData (Datum, unitRedeemer)
import Contract.Prelude (discard)
import Contract.Scripts (Validator)
import Contract.Value (CurrencySymbol)
import Dao.Utils.Query
  ( QueryType(Spend)
  , UtxoInfo
  , findScriptUtxoBySymbol
  )
import Type.Proxy (Proxy(Proxy))

type TreasuryInfo = UtxoInfo Datum

-- | Spend the treasury UTXO
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

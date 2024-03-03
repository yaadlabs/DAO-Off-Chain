module Dao.Component.Tally.Params (mkTallyConfig) where

import Contract.Value (CurrencySymbol, TokenName)
import ScriptArguments.Types (TallyPolicyParams(TallyPolicyParams))

mkTallyConfig ::
  CurrencySymbol ->
  CurrencySymbol ->
  TokenName ->
  TokenName ->
  TallyPolicyParams
mkTallyConfig configSymbol indexSymbol configTokenName indexTokenName =
  TallyPolicyParams
    { tpIndexSymbol: indexSymbol
    , tpConfigTokenName: configTokenName
    , tpConfigSymbol: configSymbol
    , tpIndexTokenName: indexTokenName
    }

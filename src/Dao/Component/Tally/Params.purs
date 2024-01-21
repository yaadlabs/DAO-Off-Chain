module Dao.Component.Tally.Params (mkTallyConfig) where

import Contract.Value (CurrencySymbol, TokenName)
import ScriptArguments.Types (TallyNftConfig(TallyNftConfig))

mkTallyConfig ::
  CurrencySymbol -> CurrencySymbol -> TokenName -> TokenName -> TallyNftConfig
mkTallyConfig configSymbol indexSymbol configTokenName indexTokenName =
  TallyNftConfig
    { tncIndexNftPolicyId: indexSymbol
    , tncConfigNftTokenName: configTokenName
    , tncConfigNftCurrencySymbol: configSymbol
    , tncIndexNftTokenName: indexTokenName
    }

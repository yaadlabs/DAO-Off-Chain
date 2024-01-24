module Scripts.TreasuryValidator
  ( unappliedTreasuryValidator
  , unappliedTreasuryValidatorDebug
  ) where

import Contract.Monad (Contract)
import Contract.Scripts (Validator)
import ScriptArguments.Types
  ( ConfigurationValidatorConfig(ConfigurationValidatorConfig)
  )
import Scripts.Utils (mkUnappliedValidator)

unappliedTreasuryValidator :: ConfigurationValidatorConfig -> Contract Validator
unappliedTreasuryValidator = mkUnappliedValidator
  "./scripts/Json/Optimised/TreasuryValidator.json"

unappliedTreasuryValidatorDebug ::
  ConfigurationValidatorConfig -> Contract Validator
unappliedTreasuryValidatorDebug = mkUnappliedValidator
  "./scripts/Json/Debug/TreasuryValidator.json"

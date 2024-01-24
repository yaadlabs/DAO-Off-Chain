module Scripts.ConfigValidator
  ( unappliedConfigValidator
  , unappliedConfigValidatorDebug
  ) where

import Contract.Monad (Contract)
import Contract.PlutusData (toData)
import Contract.Scripts (Validator)
import ScriptArguments.Types
  ( ConfigurationValidatorConfig(ConfigurationValidatorConfig)
  )
import Scripts.Utils (mkUnappliedValidator)

unappliedConfigValidator :: ConfigurationValidatorConfig -> Contract Validator
unappliedConfigValidator = mkUnappliedValidator
  "./scripts/Json/Optimised/ConfigValidator.json"

unappliedConfigValidatorDebug ::
  ConfigurationValidatorConfig -> Contract Validator
unappliedConfigValidatorDebug = mkUnappliedValidator
  "./scripts/Json/Debug/ConfigValidator.json"

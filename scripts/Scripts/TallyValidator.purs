module Scripts.TallyValidator
  ( unappliedTallyValidator
  , unappliedTallyValidatorDebug
  ) where

import Contract.Monad (Contract)
import Contract.Scripts (Validator)
import ScriptArguments.Types
  ( ConfigurationValidatorConfig(ConfigurationValidatorConfig)
  )
import Scripts.Utils (mkUnappliedValidator)

unappliedTallyValidator :: ConfigurationValidatorConfig -> Contract Validator
unappliedTallyValidator = mkUnappliedValidator
  "./scripts/Json/Optimised/TallyValidator.json"

unappliedTallyValidatorDebug ::
  ConfigurationValidatorConfig -> Contract Validator
unappliedTallyValidatorDebug = mkUnappliedValidator
  "./scripts/Json/Debug/TallyValidator.json"

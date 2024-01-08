module Scripts.ConfigValidator
  ( unappliedConfigValidator
  ) where

import Contract.Monad (Contract)
import Contract.PlutusData (toData)
import Contract.Scripts (Validator)
import LambdaBuffers.ApplicationTypes.Arguments (ConfigurationValidatorConfig)
import Scripts.Utils (mkUnappliedValidator)

unappliedConfigValidator :: ConfigurationValidatorConfig -> Contract Validator
unappliedConfigValidator = mkUnappliedValidator
  "./scripts/Json/ConfigValidator.json"

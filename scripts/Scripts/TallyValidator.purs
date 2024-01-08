module Scripts.TallyValidator
  ( unappliedTallyValidator
  ) where

import Contract.Monad (Contract)
import Contract.Scripts (Validator)
import LambdaBuffers.ApplicationTypes.Arguments
  ( ConfigurationValidatorConfig
  )
import Scripts.Utils (mkUnappliedValidator)

unappliedTallyValidator :: ConfigurationValidatorConfig -> Contract Validator
unappliedTallyValidator = mkUnappliedValidator
  "./scripts/Json/Optimised/TallyValidator.json"

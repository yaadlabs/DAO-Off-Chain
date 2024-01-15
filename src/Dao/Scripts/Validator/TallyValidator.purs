module Dao.Scripts.Validator.TallyValidator
  ( unappliedTallyValidator
  , unappliedTallyValidatorDebug
  ) where

import Contract.Monad (Contract)
import Contract.Scripts (Validator)
import Dao.Scripts.Utils (mkUnappliedValidator)
import LambdaBuffers.ApplicationTypes.Arguments
  ( ConfigurationValidatorConfig
  )

unappliedTallyValidator :: ConfigurationValidatorConfig -> Contract Validator
unappliedTallyValidator = mkUnappliedValidator
  "./src/Dao/Scripts/Json/Optimised/TallyValidator.json"

unappliedTallyValidatorDebug ::
  ConfigurationValidatorConfig -> Contract Validator
unappliedTallyValidatorDebug = mkUnappliedValidator
  "./src/Dao/Scripts/Json/Debug/TallyValidator.json"

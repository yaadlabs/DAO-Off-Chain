module Dao.Scripts.Validator.ConfigValidator
  ( unappliedConfigValidator
  , unappliedConfigValidatorDebug
  ) where

import Contract.Monad (Contract)
import Contract.PlutusData (toData)
import Contract.Scripts (Validator)
import Dao.Scripts.Utils (mkUnappliedValidator)
import LambdaBuffers.ApplicationTypes.Arguments (ConfigurationValidatorConfig)

unappliedConfigValidator :: ConfigurationValidatorConfig -> Contract Validator
unappliedConfigValidator = mkUnappliedValidator
  "./src/Dao/Scripts/Json/Optimised/ConfigValidator.json"

unappliedConfigValidatorDebug ::
  ConfigurationValidatorConfig -> Contract Validator
unappliedConfigValidatorDebug = mkUnappliedValidator
  "./src/Dao/Scripts/Json/Debug/ConfigValidator.json"

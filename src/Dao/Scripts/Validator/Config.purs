module Dao.Scripts.Validator.Config
  ( unappliedConfigValidator
  , unappliedConfigValidatorDebug
  ) where

import Contract.Monad (Contract)
import Contract.PlutusData (toData)
import Contract.Scripts (Validator)
import Dao.Scripts.Utils (mkUnappliedValidator)
import ScriptArguments.Types (ValidatorParams)

unappliedConfigValidator :: ValidatorParams -> Contract Validator
unappliedConfigValidator = mkUnappliedValidator
  "./src/Dao/Scripts/Json/Optimised/ConfigValidator.json"

unappliedConfigValidatorDebug ::
  ValidatorParams -> Contract Validator
unappliedConfigValidatorDebug = mkUnappliedValidator
  "./src/Dao/Scripts/Json/Debug/ConfigValidator.json"

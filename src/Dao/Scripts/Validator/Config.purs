module Dao.Scripts.Validator.Config
  ( unappliedConfigValidator
  , unappliedConfigValidatorDebug
  ) where

import Contract.Monad (Contract)
import Contract.PlutusData (toData)
import Contract.Scripts (Validator)
import Dao.Scripts.Serialized.Debug as DebugScripts
import Dao.Scripts.Serialized.Optimized as OptimizedScripts
import Dao.Scripts.Utils (mkUnappliedValidator')
import ScriptArguments.Types (ValidatorParams)

unappliedConfigValidator :: ValidatorParams -> Contract Validator
unappliedConfigValidator = mkUnappliedValidator' OptimizedScripts.configValidator

unappliedConfigValidatorDebug ::
  ValidatorParams -> Contract Validator
unappliedConfigValidatorDebug = mkUnappliedValidator' DebugScripts.configValidator

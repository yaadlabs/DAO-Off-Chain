module Dao.Scripts.Validator.Tally
  ( unappliedTallyValidator
  , unappliedTallyValidatorDebug
  ) where

import Contract.Monad (Contract)
import Contract.Scripts (Validator)
import Dao.Scripts.Serialized.Debug as DebugScripts
import Dao.Scripts.Serialized.Optimized as OptimizedScripts
import Dao.Scripts.Utils (mkUnappliedValidator')
import ScriptArguments.Types (ValidatorParams)

unappliedTallyValidator :: ValidatorParams -> Contract Validator
unappliedTallyValidator = mkUnappliedValidator' OptimizedScripts.tallyValidator

unappliedTallyValidatorDebug ::
  ValidatorParams -> Contract Validator
unappliedTallyValidatorDebug = mkUnappliedValidator' DebugScripts.tallyValidator

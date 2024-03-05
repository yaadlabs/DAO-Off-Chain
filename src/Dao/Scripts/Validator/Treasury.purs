module Dao.Scripts.Validator.Treasury
  ( unappliedTreasuryValidator
  , unappliedTreasuryValidatorDebug
  ) where

import Contract.Monad (Contract)
import Contract.Scripts (Validator)
import Dao.Scripts.Serialized.Debug as DebugScripts
import Dao.Scripts.Serialized.Optimized as OptimizedScripts
import Dao.Scripts.Utils (mkUnappliedValidator')
import ScriptArguments.Types (ValidatorParams)

unappliedTreasuryValidator :: ValidatorParams -> Contract Validator
unappliedTreasuryValidator = mkUnappliedValidator' OptimizedScripts.treasuryValidator

unappliedTreasuryValidatorDebug ::
  ValidatorParams -> Contract Validator
unappliedTreasuryValidatorDebug = mkUnappliedValidator' DebugScripts.treasuryValidator

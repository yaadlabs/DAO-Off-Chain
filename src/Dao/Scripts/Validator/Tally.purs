module Dao.Scripts.Validator.Tally
  ( unappliedTallyValidator
  , unappliedTallyValidatorDebug
  ) where

import Contract.Monad (Contract)
import Contract.Scripts (Validator)
import Dao.Scripts.Utils (mkUnappliedValidator)
import ScriptArguments.Types (ValidatorParams)

unappliedTallyValidator :: ValidatorParams -> Contract Validator
unappliedTallyValidator = mkUnappliedValidator
  "./src/Dao/Scripts/Json/Optimised/TallyValidator.json"

unappliedTallyValidatorDebug ::
  ValidatorParams -> Contract Validator
unappliedTallyValidatorDebug = mkUnappliedValidator
  "./src/Dao/Scripts/Json/Debug/TallyValidator.json"

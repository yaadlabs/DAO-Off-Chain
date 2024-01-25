module Dao.Scripts.Validator.Treasury
  ( unappliedTreasuryValidator
  , unappliedTreasuryValidatorDebug
  ) where

import Contract.Monad (Contract)
import Contract.Scripts (Validator)
import Dao.Scripts.Utils (mkUnappliedValidator)
import ScriptArguments.Types (ConfigurationValidatorConfig)

unappliedTreasuryValidator :: ConfigurationValidatorConfig -> Contract Validator
unappliedTreasuryValidator = mkUnappliedValidator
  "./src/Dao/Scripts/Json/Optimised/TreasuryValidator.json"

unappliedTreasuryValidatorDebug ::
  ConfigurationValidatorConfig -> Contract Validator
unappliedTreasuryValidatorDebug = mkUnappliedValidator
  "./src/Dao/Scripts/Json/Debug/TreasuryValidator.json"

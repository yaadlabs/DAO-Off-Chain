module Scripts.TreasuryValidator
  ( unappliedTreasuryValidator
  ) where

import Contract.Monad (Contract)
import Contract.Scripts (Validator)
import LambdaBuffers.ApplicationTypes.Arguments (ConfigurationValidatorConfig)
import Scripts.Utils (mkUnappliedValidator)

unappliedTreasuryValidator :: ConfigurationValidatorConfig -> Contract Validator
unappliedTreasuryValidator = mkUnappliedValidator
  "./scripts/Json/Optimised/TreasuryValidator.json"

module Scripts.IndexValidator
  ( indexValidatorScript
  ) where

import Contract.Monad (Contract)
import Contract.Prelude (pure, ($))
import Contract.Scripts (Validator(Validator))
import Scripts.Utils (mkScript)

indexValidatorScript :: Contract Validator
indexValidatorScript = pure $ Validator $ mkScript
  "./scripts/Json/IndexValidator.json"

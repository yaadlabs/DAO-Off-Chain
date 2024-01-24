module Scripts.IndexValidator
  ( indexValidatorScript
  , indexValidatorScriptDebug
  ) where

import Contract.Monad (Contract)
import Contract.Prelude (pure, ($))
import Contract.Scripts (Validator(Validator))
import Scripts.Utils (mkScript)

indexValidatorScript :: Contract Validator
indexValidatorScript = pure $ Validator $ mkScript
  "./scripts/Json/Optimised/IndexValidator.json"

indexValidatorScriptDebug :: Contract Validator
indexValidatorScriptDebug = pure $ Validator $ mkScript
  "./scripts/Json/Debug/IndexValidator.json"

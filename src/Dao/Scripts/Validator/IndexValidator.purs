module Dao.Scripts.Validator.IndexValidator
  ( indexValidatorScript
  , indexValidatorScriptDebug
  ) where

import Contract.Monad (Contract)
import Contract.Prelude (pure, ($))
import Contract.Scripts (Validator(Validator))
import Dao.Scripts.Utils (mkScript)

indexValidatorScript :: Contract Validator
indexValidatorScript = pure $ Validator $ mkScript
  "./src/Dao/Scripts/Json/Optimised/IndexValidator.json"

indexValidatorScriptDebug :: Contract Validator
indexValidatorScriptDebug = pure $ Validator $ mkScript
  "./src/Dao/Scripts/Json/Debug/IndexValidator.json"

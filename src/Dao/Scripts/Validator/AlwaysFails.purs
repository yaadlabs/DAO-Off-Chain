module Dao.Scripts.Validator.AlwaysFails
  ( alwaysFailsValidatorScript
  ) where

import Contract.Monad (Contract)
import Contract.Prelude (pure, ($))
import Contract.Scripts (Validator(Validator))
import Dao.Scripts.Utils (mkScript)

alwaysFailsValidatorScript :: Contract Validator
alwaysFailsValidatorScript = pure $ Validator $ mkScript
  "./src/Dao/Scripts/Json/Optimised/AlwaysFailsValidator.json"

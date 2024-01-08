-- Just for debugging, will remove later
module Scripts.AlwaysSucceeds
  ( alwaysSucceedsValidatorScript
  ) where

import Contract.Monad (Contract)
import Contract.Prelude (pure, ($))
import Contract.Scripts (Validator(Validator))
import Scripts.Utils (mkScript)

alwaysSucceedsValidatorScript :: Contract Validator
alwaysSucceedsValidatorScript = pure $ Validator $ mkScript
  "./scripts/Json/Optimised/AlwaysSucceedsValidator.json"

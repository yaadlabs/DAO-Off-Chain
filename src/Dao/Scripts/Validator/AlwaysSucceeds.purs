-- Just for debugging, will remove later
module Dao.Scripts.Validator.AlwaysSucceeds
  ( alwaysSucceedsValidatorScript
  ) where

import Contract.Monad (Contract)
import Contract.Prelude (pure, ($))
import Contract.Scripts (Validator(Validator))
import Dao.Scripts.Serialized.Optimized as OptimizedScripts
import Dao.Scripts.Utils (mkScript')

alwaysSucceedsValidatorScript :: Contract Validator
alwaysSucceedsValidatorScript = pure $ Validator $ mkScript' OptimizedScripts.alwaysSucceedsValidator

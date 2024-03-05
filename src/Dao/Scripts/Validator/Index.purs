module Dao.Scripts.Validator.Index
  ( indexValidatorScript
  , indexValidatorScriptDebug
  ) where

import Contract.Monad (Contract)
import Contract.Prelude (pure, ($))
import Contract.Scripts (Validator(Validator))
import Dao.Scripts.Serialized.Debug as DebugScripts
import Dao.Scripts.Serialized.Optimized as OptimizedScripts
import Dao.Scripts.Utils (mkScript')

indexValidatorScript :: Contract Validator
indexValidatorScript = pure $ Validator $ mkScript' OptimizedScripts.indexValidator

indexValidatorScriptDebug :: Contract Validator
indexValidatorScriptDebug = pure $ Validator $ mkScript' DebugScripts.indexValidator

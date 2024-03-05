module Dao.Scripts.Validator.Vote
  ( unappliedVoteValidator
  , unappliedVoteValidatorDebug
  ) where

import Contract.Monad (Contract)
import Contract.Scripts (Validator)
import Dao.Scripts.Serialized.Debug as DebugScripts
import Dao.Scripts.Serialized.Optimized as OptimizedScripts
import Dao.Scripts.Utils (mkUnappliedValidator')
import ScriptArguments.Types (ValidatorParams)

unappliedVoteValidator :: ValidatorParams -> Contract Validator
unappliedVoteValidator = mkUnappliedValidator' OptimizedScripts.voteValidator

unappliedVoteValidatorDebug ::
  ValidatorParams -> Contract Validator
unappliedVoteValidatorDebug = mkUnappliedValidator' DebugScripts.voteValidator

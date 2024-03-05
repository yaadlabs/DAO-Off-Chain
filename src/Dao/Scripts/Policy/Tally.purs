module Dao.Scripts.Policy.Tally
  ( unappliedTallyPolicy
  , unappliedTallyPolicyDebug
  ) where

import Contract.Monad (Contract)
import Contract.Scripts (MintingPolicy)
import Dao.Scripts.Serialized.Debug as DebugScripts
import Dao.Scripts.Serialized.Optimized as OptimizedScripts
import Dao.Scripts.Utils (mkUnappliedPolicy')
import ScriptArguments.Types (TallyPolicyParams)

unappliedTallyPolicy :: TallyPolicyParams -> Contract MintingPolicy
unappliedTallyPolicy = mkUnappliedPolicy' OptimizedScripts.tallyPolicy

unappliedTallyPolicyDebug :: TallyPolicyParams -> Contract MintingPolicy
unappliedTallyPolicyDebug = mkUnappliedPolicy' DebugScripts.tallyPolicy

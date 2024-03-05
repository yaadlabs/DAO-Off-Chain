module Dao.Scripts.Policy.Index
  ( unappliedIndexPolicy
  , unappliedIndexPolicyDebug
  ) where

import Contract.Monad (Contract)
import Contract.Scripts (MintingPolicy)
import Dao.Scripts.Serialized.Debug as DebugScripts
import Dao.Scripts.Serialized.Optimized as OptimizedScripts
import Dao.Scripts.Utils (mkUnappliedPolicy')
import ScriptArguments.Types (IndexPolicyParams)

unappliedIndexPolicy :: IndexPolicyParams -> Contract MintingPolicy
unappliedIndexPolicy = mkUnappliedPolicy' OptimizedScripts.indexPolicy

unappliedIndexPolicyDebug :: IndexPolicyParams -> Contract MintingPolicy
unappliedIndexPolicyDebug = mkUnappliedPolicy' DebugScripts.indexPolicy

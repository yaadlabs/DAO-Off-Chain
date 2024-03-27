module Dao.Scripts.Policy.Config
  ( unappliedConfigPolicy
  , unappliedConfigPolicyDebug
  ) where

import Contract.Monad (Contract)
import Contract.Scripts (MintingPolicy)
import Dao.Scripts.Utils (mkUnappliedPolicy')
import Dao.Scripts.Serialized.Debug as DebugScripts
import Dao.Scripts.Serialized.Optimized as OptimizedScripts
import ScriptArguments.Types (ConfigPolicyParams)

unappliedConfigPolicy :: ConfigPolicyParams -> Contract MintingPolicy
unappliedConfigPolicy = mkUnappliedPolicy' OptimizedScripts.configPolicy

unappliedConfigPolicyDebug :: ConfigPolicyParams -> Contract MintingPolicy
unappliedConfigPolicyDebug = mkUnappliedPolicy' DebugScripts.configPolicy

module Scripts.TallyPolicy
  ( unappliedTallyPolicy
  , unappliedTallyPolicyDebug
  ) where

import Contract.Monad (Contract)
import Contract.Scripts (MintingPolicy)
import ScriptArguments.Types (TallyNftConfig)
import Scripts.Utils (mkUnappliedPolicy)

unappliedTallyPolicy :: TallyNftConfig -> Contract MintingPolicy
unappliedTallyPolicy = mkUnappliedPolicy
  "./scripts/Json/Optimised/TallyPolicy.json"

unappliedTallyPolicyDebug :: TallyNftConfig -> Contract MintingPolicy
unappliedTallyPolicyDebug = mkUnappliedPolicy
  "./scripts/Json/Debug/TallyPolicy.json"

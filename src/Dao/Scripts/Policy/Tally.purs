module Dao.Scripts.Policy.Tally
  ( unappliedTallyPolicy
  , unappliedTallyPolicyDebug
  ) where

import Contract.Monad (Contract)
import Contract.Scripts (MintingPolicy)
import Dao.Scripts.Utils (mkUnappliedPolicy)
import ScriptArguments.Types (TallyNftConfig)

unappliedTallyPolicy :: TallyNftConfig -> Contract MintingPolicy
unappliedTallyPolicy = mkUnappliedPolicy
  "./src/Dao/Scripts/Json/Optimised/TallyPolicy.json"

unappliedTallyPolicyDebug :: TallyNftConfig -> Contract MintingPolicy
unappliedTallyPolicyDebug = mkUnappliedPolicy
  "./src/Dao/Scripts/Json/Debug/TallyPolicy.json"

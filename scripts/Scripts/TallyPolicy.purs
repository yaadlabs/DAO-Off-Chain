module Scripts.TallyPolicy
  ( unappliedTallyPolicy
  ) where

import Contract.Monad (Contract)
import Contract.Scripts (MintingPolicy)
import ScriptArguments.Types (TallyNftConfig)
import Scripts.Utils (mkUnappliedPolicy)

unappliedTallyPolicy :: TallyNftConfig -> Contract MintingPolicy
unappliedTallyPolicy = mkUnappliedPolicy "./scripts/Json/TallyPolicy.json"

module Scripts.IndexPolicy
  ( unappliedIndexPolicy
  , unappliedIndexPolicyDebug
  ) where

import Contract.Monad (Contract)
import Contract.Scripts (MintingPolicy)
import ScriptArguments.Types (IndexNftConfig)
import Scripts.Utils (mkUnappliedPolicy)

unappliedIndexPolicy :: IndexNftConfig -> Contract MintingPolicy
unappliedIndexPolicy = mkUnappliedPolicy
  "./scripts/Json/Optimised/IndexPolicy.json"

unappliedIndexPolicyDebug :: IndexNftConfig -> Contract MintingPolicy
unappliedIndexPolicyDebug = mkUnappliedPolicy
  "./scripts/Json/Debug/IndexPolicy.json"

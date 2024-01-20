module Scripts.IndexPolicy
  ( unappliedIndexPolicy
  ) where

import Contract.Monad (Contract)
import Contract.Scripts (MintingPolicy)
import ScriptArguments.Types (IndexNftConfig)
import Scripts.Utils (mkUnappliedPolicy)

unappliedIndexPolicy :: IndexNftConfig -> Contract MintingPolicy
unappliedIndexPolicy = mkUnappliedPolicy "./scripts/Json/IndexPolicy.json"

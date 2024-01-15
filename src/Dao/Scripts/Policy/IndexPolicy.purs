module Dao.Scripts.Policy.IndexPolicy
  ( unappliedIndexPolicy
  , unappliedIndexPolicyDebug
  ) where

import Contract.Monad (Contract)
import Contract.Scripts (MintingPolicy)
import Dao.Scripts.Utils (mkUnappliedPolicy)
import ScriptArguments.Types (IndexNftConfig)

unappliedIndexPolicy :: IndexNftConfig -> Contract MintingPolicy
unappliedIndexPolicy = mkUnappliedPolicy
  "./src/Dao/Scripts/Json/Optimised/IndexPolicy.json"

unappliedIndexPolicyDebug :: IndexNftConfig -> Contract MintingPolicy
unappliedIndexPolicyDebug = mkUnappliedPolicy
  "./src/Dao/Scripts/Json/Debug/IndexPolicy.json"

module Dao.Scripts.Policy.Index
  ( unappliedIndexPolicy
  , unappliedIndexPolicyDebug
  ) where

import Contract.Monad (Contract)
import Contract.Scripts (MintingPolicy)
import Dao.Scripts.Utils (mkUnappliedPolicy)
import ScriptArguments.Types (IndexPolicyParams)

unappliedIndexPolicy :: IndexPolicyParams -> Contract MintingPolicy
unappliedIndexPolicy = mkUnappliedPolicy
  "./src/Dao/Scripts/Json/Optimised/IndexPolicy.json"

unappliedIndexPolicyDebug :: IndexPolicyParams -> Contract MintingPolicy
unappliedIndexPolicyDebug = mkUnappliedPolicy
  "./src/Dao/Scripts/Json/Debug/IndexPolicy.json"

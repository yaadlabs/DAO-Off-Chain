module Dao.Scripts.Policy.Config
  ( unappliedConfigPolicy
  , unappliedConfigPolicyDebug
  ) where

import Contract.Monad (Contract)
import Contract.Scripts (MintingPolicy)
import Dao.Scripts.Utils (mkUnappliedPolicy)
import ScriptArguments.Types (ConfigPolicyParams)

unappliedConfigPolicy :: ConfigPolicyParams -> Contract MintingPolicy
unappliedConfigPolicy = mkUnappliedPolicy
  "./src/Dao/Scripts/Json/Optimised/ConfigPolicy.json"

unappliedConfigPolicyDebug :: ConfigPolicyParams -> Contract MintingPolicy
unappliedConfigPolicyDebug = mkUnappliedPolicy
  "./src/Dao/Scripts/Json/Debug/ConfigPolicy.json"

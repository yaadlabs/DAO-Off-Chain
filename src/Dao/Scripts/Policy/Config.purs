module Dao.Scripts.Policy.Config
  ( unappliedConfigPolicy
  , unappliedConfigPolicyDebug
  ) where

import Contract.Monad (Contract)
import Contract.Scripts (MintingPolicy)
import Dao.Scripts.Utils (mkUnappliedPolicy)
import ScriptArguments.Types (NftConfig)

unappliedConfigPolicy :: NftConfig -> Contract MintingPolicy
unappliedConfigPolicy = mkUnappliedPolicy
  "./src/Dao/Scripts/Json/Optimised/ConfigPolicy.json"

unappliedConfigPolicyDebug :: NftConfig -> Contract MintingPolicy
unappliedConfigPolicyDebug = mkUnappliedPolicy
  "./src/Dao/Scripts/Json/Debug/ConfigPolicy.json"

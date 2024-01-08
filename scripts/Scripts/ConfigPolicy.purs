module Scripts.ConfigPolicy
  ( unappliedConfigPolicy
  , unappliedConfigPolicyDebug
  ) where

import Contract.Monad (Contract)
import Contract.Scripts (MintingPolicy)
import LambdaBuffers.ApplicationTypes.Arguments (NftConfig)
import Scripts.Utils (mkUnappliedPolicy)

unappliedConfigPolicy :: NftConfig -> Contract MintingPolicy
unappliedConfigPolicy = mkUnappliedPolicy
  "./scripts/Json/Optimised/ConfigPolicy.json"

unappliedConfigPolicyDebug :: NftConfig -> Contract MintingPolicy
unappliedConfigPolicyDebug = mkUnappliedPolicy
  "./scripts/Json/Debug/ConfigPolicy.json"

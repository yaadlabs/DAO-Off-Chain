module Scripts.ConfigPolicy
  ( unappliedConfigPolicy
  ) where

import Contract.Monad (Contract)
import Contract.Scripts (MintingPolicy)
import LambdaBuffers.ApplicationTypes.Arguments (NftConfig)
import Scripts.Utils (mkUnappliedPolicy)

unappliedConfigPolicy :: NftConfig -> Contract MintingPolicy
unappliedConfigPolicy = mkUnappliedPolicy "./scripts/Json/ConfigPolicy.json"

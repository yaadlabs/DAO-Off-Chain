module Scripts.FungiblePolicy
  ( unappliedFungiblePolicy
  , unappliedFungiblePolicyDebug
  ) where

import Contract.Monad (Contract)
import Contract.Scripts (MintingPolicy)
import JS.BigInt (BigInt)
import Scripts.Utils (mkUnappliedPolicy)

unappliedFungiblePolicy :: BigInt -> Contract MintingPolicy
unappliedFungiblePolicy = mkUnappliedPolicy
  "./scripts/Json/Optimised/FungiblePolicy.json"

unappliedFungiblePolicyDebug :: BigInt -> Contract MintingPolicy
unappliedFungiblePolicyDebug = mkUnappliedPolicy
  "./scripts/Json/Debug/FungiblePolicy.json"

module Dao.Scripts.Policy.Fungible
  ( unappliedFungiblePolicy
  , unappliedFungiblePolicyDebug
  ) where

import Contract.Monad (Contract)
import Contract.Scripts (MintingPolicy)
import Dao.Scripts.Utils (mkUnappliedPolicy)
import JS.BigInt (BigInt)

unappliedFungiblePolicy :: BigInt -> Contract MintingPolicy
unappliedFungiblePolicy = mkUnappliedPolicy
  ".src/Dao/Scripts/Json/Optimised/FungiblePolicy.json"

unappliedFungiblePolicyDebug :: BigInt -> Contract MintingPolicy
unappliedFungiblePolicyDebug = mkUnappliedPolicy
  "./src/Dao/Scripts/Json/Debug/FungiblePolicy.json"

module Dao.Scripts.Policy.Treasury
  ( unappliedTreasuryPolicy
  , unappliedTreasuryPolicyDebug
  ) where

import Contract.Monad (Contract)
import Contract.Scripts (MintingPolicy)
import Contract.Transaction (TransactionInput)
import Dao.Scripts.Utils (mkUnappliedPolicy)

unappliedTreasuryPolicy :: TransactionInput -> Contract MintingPolicy
unappliedTreasuryPolicy = mkUnappliedPolicy
  "./src/Dao/Scripts/Json/Optimised/TreasuryPolicy.json"

unappliedTreasuryPolicyDebug :: TransactionInput -> Contract MintingPolicy
unappliedTreasuryPolicyDebug = mkUnappliedPolicy
  "./src/Dao/Scripts/Json/Debug/TreasuryPolicy.json"

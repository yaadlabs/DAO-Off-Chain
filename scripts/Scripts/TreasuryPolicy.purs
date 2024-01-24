module Scripts.TreasuryPolicy
  ( unappliedTreasuryPolicy
  , unappliedTreasuryPolicyDebug
  ) where

import Contract.Monad (Contract)
import Contract.Scripts (MintingPolicy)
import Contract.Transaction (TransactionInput)
import Scripts.Utils (mkUnappliedPolicy)

unappliedTreasuryPolicy :: TransactionInput -> Contract MintingPolicy
unappliedTreasuryPolicy = mkUnappliedPolicy
  "./scripts/Json/Optimised/TreasuryPolicy.json"

unappliedTreasuryPolicyDebug :: TransactionInput -> Contract MintingPolicy
unappliedTreasuryPolicyDebug = mkUnappliedPolicy
  "./scripts/Json/Debug/TreasuryPolicy.json"

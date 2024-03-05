module Dao.Scripts.Policy.Treasury
  ( unappliedTreasuryPolicy
  , unappliedTreasuryPolicyDebug
  ) where

import Contract.Monad (Contract)
import Contract.Scripts (MintingPolicy)
import Contract.Transaction (TransactionInput)
import Dao.Scripts.Serialized.Debug as DebugScripts
import Dao.Scripts.Serialized.Optimized as OptimizedScripts
import Dao.Scripts.Utils (mkUnappliedPolicy')

unappliedTreasuryPolicy :: TransactionInput -> Contract MintingPolicy
unappliedTreasuryPolicy = mkUnappliedPolicy' OptimizedScripts.treasuryPolicy

unappliedTreasuryPolicyDebug :: TransactionInput -> Contract MintingPolicy
unappliedTreasuryPolicyDebug = mkUnappliedPolicy' DebugScripts.treasuryPolicy

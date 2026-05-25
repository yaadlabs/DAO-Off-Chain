module Dao.Scripts.Policy
  ( unappliedConfigPolicy
  , fungiblePolicy
  , unappliedIndexPolicy
  , unappliedTallyPolicy
  , unappliedTreasuryPolicy
  , upgradePolicy
  , unappliedVotePolicy
  , voteNftPolicy
  ) where

import Cardano.Types (PlutusScript)
import Contract.Monad (Contract)
import Contract.Prelude (pure, ($))
import Contract.Transaction (TransactionInput)
import Dao.Scripts.Serialized.Optimised as Optimised
import Dao.Scripts.Utils (mkScript', mkUnappliedPolicy')
import ScriptArguments.Types
  ( ConfigPolicyParams
  , IndexPolicyParams
  , TallyPolicyParams
  , ValidatorParams
  )

unappliedConfigPolicy :: ConfigPolicyParams -> Contract PlutusScript
unappliedConfigPolicy = mkUnappliedPolicy' Optimised.configPolicy

fungiblePolicy :: Contract PlutusScript
fungiblePolicy = pure $ mkScript' Optimised.fungiblePolicy

unappliedIndexPolicy :: IndexPolicyParams -> Contract PlutusScript
unappliedIndexPolicy = mkUnappliedPolicy' Optimised.indexPolicy

unappliedTallyPolicy :: TallyPolicyParams -> Contract PlutusScript
unappliedTallyPolicy = mkUnappliedPolicy' Optimised.tallyPolicy

unappliedTreasuryPolicy :: TransactionInput -> Contract PlutusScript
unappliedTreasuryPolicy = mkUnappliedPolicy' Optimised.treasuryPolicy

-- | The upgrade proposal requires a policy script to be included
-- | in the transaction as well, with the intention of delegating some
-- | of the validation logic to this policy.
-- | We use an always succeeds minting policy as a placeholder for now.
upgradePolicy :: Contract PlutusScript
upgradePolicy = pure $ mkScript' Optimised.alwaysMints

unappliedVotePolicy :: ValidatorParams -> Contract PlutusScript
unappliedVotePolicy = mkUnappliedPolicy' Optimised.votePolicy

voteNftPolicy :: Contract PlutusScript
voteNftPolicy = pure $ mkScript' Optimised.voteNftPolicy


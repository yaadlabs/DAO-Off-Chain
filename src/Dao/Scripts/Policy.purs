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

import Contract.Monad (Contract)
import Contract.Prelude (pure, ($))
import Contract.Scripts (MintingPolicy(PlutusMintingPolicy))
import Contract.Transaction (TransactionInput)
import Dao.Scripts.Serialized.Debug as Debug
import Dao.Scripts.Serialized.Optimised as Optimised
import Dao.Scripts.Utils (mkUnappliedPolicy', mkScript')
import ScriptArguments.Types
  ( ConfigPolicyParams
  , IndexPolicyParams
  , TallyPolicyParams
  , ValidatorParams
  )

unappliedConfigPolicy :: ConfigPolicyParams -> Contract MintingPolicy
unappliedConfigPolicy = mkUnappliedPolicy' Debug.configPolicy

fungiblePolicy :: Contract MintingPolicy
fungiblePolicy = pure $ PlutusMintingPolicy $ mkScript' Optimised.fungiblePolicy

unappliedIndexPolicy :: IndexPolicyParams -> Contract MintingPolicy
unappliedIndexPolicy = mkUnappliedPolicy' Debug.indexPolicy

unappliedTallyPolicy :: TallyPolicyParams -> Contract MintingPolicy
unappliedTallyPolicy = mkUnappliedPolicy' Optimised.tallyPolicy

unappliedTreasuryPolicy :: TransactionInput -> Contract MintingPolicy
unappliedTreasuryPolicy = mkUnappliedPolicy' Debug.treasuryPolicy

-- | The upgrade proposal requires a policy script to be included
-- | in the transaction as well, with the intention of delegating some
-- | of the validation logic to this policy.
-- | We use an always succeeds minting policy as a placeholder for now.
upgradePolicy :: Contract MintingPolicy
upgradePolicy = pure $ PlutusMintingPolicy $ mkScript' Optimised.alwaysMints

unappliedVotePolicy :: ValidatorParams -> Contract MintingPolicy
unappliedVotePolicy = mkUnappliedPolicy' Optimised.votePolicy

voteNftPolicy :: Contract MintingPolicy
voteNftPolicy = pure $ PlutusMintingPolicy $ mkScript' Optimised.voteNftPolicy


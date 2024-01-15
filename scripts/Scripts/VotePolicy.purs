module Scripts.VotePolicy
  ( unappliedVotePolicy
  , unappliedVotePolicyDebug
  ) where

import Contract.Monad (Contract)
import Contract.Scripts (MintingPolicy)
import LambdaBuffers.ApplicationTypes.Arguments (ConfigurationValidatorConfig)
import Scripts.Utils (mkUnappliedPolicy)

unappliedVotePolicy :: ConfigurationValidatorConfig -> Contract MintingPolicy
unappliedVotePolicy = mkUnappliedPolicy
  "./scripts/Json/Optimised/VotePolicy.json"

unappliedVotePolicyDebug ::
  ConfigurationValidatorConfig -> Contract MintingPolicy
unappliedVotePolicyDebug = mkUnappliedPolicy
  "./scripts/Json/Debug/VotePolicy.json"

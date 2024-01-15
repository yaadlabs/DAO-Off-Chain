module Dao.Scripts.Policy.VotePolicy
  ( unappliedVotePolicy
  , unappliedVotePolicyDebug
  ) where

import Contract.Monad (Contract)
import Contract.Scripts (MintingPolicy)
import Dao.Scripts.Utils (mkUnappliedPolicy)
import LambdaBuffers.ApplicationTypes.Arguments (ConfigurationValidatorConfig)

unappliedVotePolicy :: ConfigurationValidatorConfig -> Contract MintingPolicy
unappliedVotePolicy = mkUnappliedPolicy
  "./src/Dao/Scripts/Json/Optimised/VotePolicy.json"

unappliedVotePolicyDebug ::
  ConfigurationValidatorConfig -> Contract MintingPolicy
unappliedVotePolicyDebug = mkUnappliedPolicy
  "./src/Dao/Scripts/Json/Debug/VotePolicy.json"

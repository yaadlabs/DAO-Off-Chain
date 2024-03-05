module Dao.Scripts.Policy.Vote
  ( unappliedVotePolicy
  , unappliedVotePolicyDebug
  ) where

import Contract.Monad (Contract)
import Contract.Scripts (MintingPolicy)
import Dao.Scripts.Serialized.Debug as DebugScripts
import Dao.Scripts.Serialized.Optimized as OptimizedScripts
import Dao.Scripts.Utils (mkUnappliedPolicy')
import ScriptArguments.Types (ValidatorParams)


unappliedVotePolicy :: ValidatorParams -> Contract MintingPolicy
unappliedVotePolicy = mkUnappliedPolicy' OptimizedScripts.voteNftPolicy

unappliedVotePolicyDebug ::
  ValidatorParams -> Contract MintingPolicy
unappliedVotePolicyDebug = mkUnappliedPolicy' DebugScripts.voteNftPolicy

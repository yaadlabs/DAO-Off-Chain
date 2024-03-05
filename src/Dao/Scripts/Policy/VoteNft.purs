module Dao.Scripts.Policy.VoteNft
  ( voteNftPolicy
  , voteNftPolicyDebug
  ) where

import Contract.Monad (Contract)
import Contract.Prelude (pure, ($))
import Contract.Scripts (MintingPolicy(PlutusMintingPolicy))
import Dao.Scripts.Serialized.Debug as DebugScripts
import Dao.Scripts.Serialized.Optimized as OptimizedScripts
import Dao.Scripts.Utils (mkScript')

voteNftPolicy :: Contract MintingPolicy
voteNftPolicy = pure $ PlutusMintingPolicy $ mkScript' OptimizedScripts.voteNftPolicy

voteNftPolicyDebug :: Contract MintingPolicy
voteNftPolicyDebug = pure $ PlutusMintingPolicy $ mkScript' DebugScripts.voteNftPolicy

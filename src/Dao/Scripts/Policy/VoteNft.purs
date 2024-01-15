module Dao.Scripts.Policy.VoteNft
  ( voteNftPolicy
  ) where

import Contract.Monad (Contract)
import Contract.Prelude (pure, ($))
import Contract.Scripts (MintingPolicy(PlutusMintingPolicy))
import Dao.Scripts.Utils (mkScript)

voteNftPolicy :: Contract MintingPolicy
voteNftPolicy = pure $ PlutusMintingPolicy $ mkScript
  "./src/Dao/Scripts/Json/Optimised/AlwaysMints.json"

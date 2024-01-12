module Scripts.VoteNft
  ( voteNftPolicy
  ) where

import Contract.Monad (Contract)
import Contract.Prelude (pure, ($))
import Contract.Scripts (MintingPolicy(PlutusMintingPolicy))
import Scripts.Utils (mkScript)

voteNftPolicy :: Contract MintingPolicy
voteNftPolicy = pure $ PlutusMintingPolicy $ mkScript
  "./scripts/Json/Optimised/AlwaysMints.json"

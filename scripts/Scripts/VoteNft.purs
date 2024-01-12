module Scripts.VoteNft
  ( voteNftPolicy
  ) where

import Contract.Prelude (pure, ($))
import Contract.Monad (Contract)
import Contract.Scripts (MintingPolicy(PlutusMintingPolicy))
import Scripts.Utils (mkScript)

voteNftPolicy :: Contract MintingPolicy
voteNftPolicy = pure $ PlutusMintingPolicy $ mkScript "./scripts/Json/Optimised/VoteNft.json"

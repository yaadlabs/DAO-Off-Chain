module Dao.Scripts.Policy.Fungible
  ( fungiblePolicy
  ) where

import Contract.Monad (Contract)
import Contract.Prelude (pure, ($))
import Contract.Scripts (MintingPolicy(PlutusMintingPolicy))
import Dao.Scripts.Utils (mkScript')
import Dao.Scripts.Serialized.Optimized as OptimizedScripts

fungiblePolicy :: Contract MintingPolicy
fungiblePolicy = pure $ PlutusMintingPolicy $ mkScript' OptimizedScripts.fungiblePolicy

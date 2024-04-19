module Dao.Scripts.Policy.Fungible
  ( fungiblePolicy
  ) where

import Contract.Monad (Contract)
import Contract.Prelude (pure, ($))
import Contract.Scripts (MintingPolicy(PlutusMintingPolicy))
import Dao.Scripts.Utils (mkScript)

fungiblePolicy :: Contract MintingPolicy
fungiblePolicy = pure $ PlutusMintingPolicy $ mkScript
  "./src/Dao/Scripts/Json/Optimised/FungiblePolicy.json"

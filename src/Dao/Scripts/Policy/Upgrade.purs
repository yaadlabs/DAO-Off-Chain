module Dao.Scripts.Policy.Upgrade
  ( upgradePolicy
  ) where

import Contract.Monad (Contract)
import Contract.Prelude (pure, ($))
import Contract.Scripts (MintingPolicy(PlutusMintingPolicy))
import Dao.Scripts.Utils (mkScript)

-- | The upgrade proposal requires a policy script to be included
-- | in the transaction as well, with the intention of delegating some
-- | of the validation logic to this policy.
-- | We use an always succeeds minting policy as a placeholder for now.
upgradePolicy :: Contract MintingPolicy
upgradePolicy = pure $ PlutusMintingPolicy $ mkScript
  "./src/Dao/Scripts/Json/Optimised/AlwaysMints.json"

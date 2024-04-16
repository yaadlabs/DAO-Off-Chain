module Dao.Utils.Constraints
  ( mustPayToPubKeyStakeAddress
  ) where

import Contract.Address (PaymentPubKeyHash, StakePubKeyHash)
import Contract.TxConstraints as Constraints
import Contract.Value (Value)
import Data.Maybe (Maybe(Just, Nothing))

-- | Borrowed from Ctl.Examples.Helpers
mustPayToPubKeyStakeAddress ::
    PaymentPubKeyHash
  -> Maybe StakePubKeyHash
  -> Value
  -> Constraints.TxConstraints
mustPayToPubKeyStakeAddress pkh Nothing =
  Constraints.mustPayToPubKey pkh
mustPayToPubKeyStakeAddress pkh (Just skh) =
  Constraints.mustPayToPubKeyAddress pkh skh

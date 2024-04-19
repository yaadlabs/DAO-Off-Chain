module Dao.Utils.Address
  ( addressToPaymentPubKeyHash
  , addressToStakePubKeyHash
  , paymentPubKeyHashToAddress
  ) where

import Contract.Address
  ( PaymentPubKeyHash(PaymentPubKeyHash)
  , StakePubKeyHash(..)
  )
import Contract.Credential
  ( Credential(PubKeyCredential)
  , StakingCredential(..)
  )
import Contract.Prelude (($))
import Ctl.Internal.Plutus.Types.Address (Address(Address))
import Data.Maybe (Maybe(Just, Nothing))

addressToPaymentPubKeyHash :: Address -> Maybe PaymentPubKeyHash
addressToPaymentPubKeyHash (Address { addressCredential }) =
  case addressCredential of
    PubKeyCredential pubKeyHash -> Just $ PaymentPubKeyHash pubKeyHash
    _ -> Nothing

addressToStakePubKeyHash :: Address -> Maybe StakePubKeyHash
addressToStakePubKeyHash (Address { addressStakingCredential }) =
  case addressStakingCredential of
    Just (StakingHash (PubKeyCredential pubKeyHash))-> Just $ StakePubKeyHash pubKeyHash
    _ -> Nothing

paymentPubKeyHashToAddress :: PaymentPubKeyHash -> Address
paymentPubKeyHashToAddress (PaymentPubKeyHash pkh) =
  Address
    { addressCredential: PubKeyCredential pkh
    , addressStakingCredential: Nothing
    }

module Dao.Utils.Address
  ( addressToPaymentPubKeyHash
  , paymentPubKeyHashToAddress
  ) where

import Contract.Address
  ( PaymentPubKeyHash(PaymentPubKeyHash)
  )
import Contract.Credential (Credential(PubKeyCredential))
import Contract.Prelude (($))
import Ctl.Internal.Plutus.Types.Address (Address(Address))
import Data.Maybe (Maybe(Just, Nothing))

addressToPaymentPubKeyHash :: Address -> Maybe PaymentPubKeyHash
addressToPaymentPubKeyHash (Address { addressCredential }) =
  case addressCredential of
    PubKeyCredential pubKeyHash -> Just $ PaymentPubKeyHash pubKeyHash
    _ -> Nothing

paymentPubKeyHashToAddress :: PaymentPubKeyHash -> Address
paymentPubKeyHashToAddress (PaymentPubKeyHash pkh) =
  Address
    { addressCredential: PubKeyCredential pkh
    , addressStakingCredential: Nothing
    }

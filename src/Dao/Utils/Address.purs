module Dao.Utils.Address
  ( addressToPaymentPubKeyHash
  , addressToStakePubKeyHash
  , paymentPubKeyHashToAddress
  , plutusAddressToPaymentPubKeyHash
  ) where

import Cardano.Plutus.Types.Address (Address) as Plutus
import Cardano.Plutus.Types.Credential (Credential(PubKeyCredential)) as Plutus
import Cardano.Types (Address, Credential(PubKeyHashCredential), NetworkId, PaymentCredential(PaymentCredential), PaymentPubKeyHash, StakeCredential(StakeCredential), StakePubKeyHash)
import Cardano.Types.Address (getPaymentCredential, getStakeCredential, mkPaymentAddress)
import Contract.Prelude (($))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap, wrap)

plutusAddressToPaymentPubKeyHash :: Plutus.Address -> Maybe PaymentPubKeyHash
plutusAddressToPaymentPubKeyHash addr =
  case (unwrap addr).addressCredential of
    Plutus.PubKeyCredential pkh ->
      Just $ wrap $ unwrap pkh
    _ ->
      Nothing

addressToPaymentPubKeyHash :: Address -> Maybe PaymentPubKeyHash
addressToPaymentPubKeyHash addr =
  case getPaymentCredential addr of
    Just (PaymentCredential (PubKeyHashCredential pkh)) ->
      Just $ wrap pkh
    _ ->
      Nothing

addressToStakePubKeyHash :: Address -> Maybe StakePubKeyHash
addressToStakePubKeyHash addr =
  case getStakeCredential addr of
    Just (StakeCredential (PubKeyHashCredential pkh)) ->
      Just $ wrap pkh
    _ ->
      Nothing

paymentPubKeyHashToAddress :: NetworkId -> PaymentPubKeyHash -> Address
paymentPubKeyHashToAddress network pkh =
  mkPaymentAddress network (wrap $ PubKeyHashCredential $ unwrap pkh)
    Nothing

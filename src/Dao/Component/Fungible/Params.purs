{-|
Module: Dao.Component.Fungible.Params
Description: Helpers create fungible workflow
-}
module Dao.Component.Fungible.Params (CreateFungibleParams(..)) where

import Contract.Address (PaymentPubKeyHash)
import Data.Newtype (class Newtype)
import JS.BigInt (BigInt)

-- | Create fungible contract paramaters
newtype CreateFungibleParams = CreateFungibleParams
  { userPkh :: PaymentPubKeyHash
  , amount :: BigInt
  }

derive instance Newtype CreateFungibleParams _

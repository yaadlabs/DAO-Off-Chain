{-|
Module: Dao.Component.Treasury.Params
Description: Treasury helpers
-}
module Dao.Component.Treasury.Params
  ( TreasuryGeneralParams(..)
  , TreasuryTripParams(..)
  ) where

import Contract.Address (Address)
import Contract.Value (CurrencySymbol, TokenName)
import Data.Newtype (class Newtype)
import JS.BigInt (BigInt)

-- | Parameters for treasury trip contract
newtype TreasuryTripParams = TreasuryTripParams
  { travelAgentAddress :: Address
  , travellerAddress :: Address
  , totalTravelCost :: BigInt
  , configSymbol :: CurrencySymbol
  , configTokenName :: TokenName
  , tallySymbol :: CurrencySymbol
  , treasurySymbol :: CurrencySymbol
  }

derive instance Newtype TreasuryTripParams _

-- | Parameters for treasury general contract
newtype TreasuryGeneralParams = TreasuryGeneralParams
  { paymentAddress :: Address
  , generalPaymentAmount :: BigInt
  , configSymbol :: CurrencySymbol
  , tallySymbol :: CurrencySymbol
  , treasurySymbol :: CurrencySymbol
  , configTokenName :: TokenName
  }

derive instance Newtype TreasuryGeneralParams _

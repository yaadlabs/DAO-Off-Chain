{-|
Module: Dao.Component.Treasury.Params
Description: Treasury helpers
-}
module Dao.Component.Treasury.Params
  ( TreasuryParamsGeneral
  , TreasuryParamsTrip
  ) where

import Contract.Address (Address)
import Contract.Value (CurrencySymbol, TokenName)
import JS.BigInt (BigInt)

-- | Parameters for treasury trip contract
type TreasuryParamsTrip =
  { travelAgentAddress :: Address
  , travellerAddress :: Address
  , totalCost :: BigInt
  }

-- | Parameters for treasury general contract
type TreasuryParamsGeneral =
  { paymentAddress :: Address
  , generalPaymentAmount :: BigInt
  , configSymbol :: CurrencySymbol
  , tallySymbol :: CurrencySymbol
  , treasurySymbol :: CurrencySymbol
  , configTokenName :: TokenName
  }

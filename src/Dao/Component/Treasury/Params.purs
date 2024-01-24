{-|
Module: Dao.Component.Treasury.Params
Description: Treasury helpers
-}
module Dao.Component.Treasury.Params
  ( TreasuryGeneralParams
  , TreasuryTripParams
  , TreasuryFundParams
  ) where

import Contract.Address (Address)
import Contract.Value (CurrencySymbol, TokenName)
import JS.BigInt (BigInt)

-- | Parameters for treasury trip contract
type TreasuryTripParams =
  { travelAgentAddress :: Address
  , travellerAddress :: Address
  , totalTravelCost :: BigInt
  , configSymbol :: CurrencySymbol
  , configTokenName :: TokenName
  , tallySymbol :: CurrencySymbol
  , treasurySymbol :: CurrencySymbol
  }

-- | Parameters for treasury general contract
type TreasuryGeneralParams =
  { configSymbol :: CurrencySymbol
  , tallySymbol :: CurrencySymbol
  , treasurySymbol :: CurrencySymbol
  , configTokenName :: TokenName
  }

-- | Parameters for treasury fund contract
type TreasuryFundParams =
  { adaAmount :: BigInt
  , configSymbol :: CurrencySymbol
  , configTokenName :: TokenName
  }

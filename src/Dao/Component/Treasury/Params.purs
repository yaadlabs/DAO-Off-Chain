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

-- | Parameters passed treasury contract
type TreasuryParamsTrip =
  { travelAgentAddress :: Address
  , travellerAddress :: Address
  , totalCost :: BigInt
  , configSymbol :: CurrencySymbol
  , configTokenName :: TokenName
  , tallySymbol :: CurrencySymbol
  , treasurySymbol :: CurrencySymbol
  }

type TreasuryParamsGeneral =
  { paymentAddress :: Address
  , totalCost :: BigInt
  }

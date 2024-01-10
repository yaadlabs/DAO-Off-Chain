{-|
Module: Dao.Component.Treasury.Params
Description: Treasury helpers
-}
module Dao.Component.Treasury.Params
  ( TreasuryParamsGeneral
  , TreasuryParamsTrip
  ) where

import Contract.Address (Address)
import JS.BigInt (BigInt)

-- | Parameters passed treasury contract
type TreasuryParamsTrip =
  { travelAgentAddress :: Address
  , travellerAddress :: Address
  , totalCost :: BigInt
  }

type TreasuryParamsGeneral =
  { paymentAddress :: Address
  , totalCost :: BigInt
  }

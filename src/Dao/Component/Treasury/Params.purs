{-|
Module: Dao.Component.Treasury.Params
Description: Treasury helpers
-}
module Dao.Component.Treasury.Params
  ( TreasuryParams
  ) where

import Contract.Address (Address)
import JS.BigInt (BigInt)

-- | Parameters passed treasury contract
type TreasuryParams =
  { travelAgentAddress :: Address
  , travellerAddress :: Address
  , totalCost :: BigInt
  }

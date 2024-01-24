{-|
Module: Dao.Component.Treasury.Params
Description: Treasury helpers
-}
module Dao.Component.Treasury.Params
  ( TreasuryParams
  , TreasuryFundParams
  ) where

import Contract.Value (CurrencySymbol, TokenName)
import JS.BigInt (BigInt)

-- | Parameters for treasury general or trip contracts
type TreasuryParams =
  { configSymbol :: CurrencySymbol
  , configTokenName :: TokenName
  , tallySymbol :: CurrencySymbol
  , treasurySymbol :: CurrencySymbol
  }

-- | Parameters for treasury fund contract
type TreasuryFundParams =
  { adaAmount :: BigInt
  , configSymbol :: CurrencySymbol
  , configTokenName :: TokenName
  }

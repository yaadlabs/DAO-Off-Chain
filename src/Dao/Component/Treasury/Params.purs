{-|
Module: Dao.Component.Treasury.Params
Description: Parameters for the treasury contracts
-}
module Dao.Component.Treasury.Params
  ( TreasuryParams(..)
  , TreasuryFundParams
  ) where

import Contract.Value (CurrencySymbol, TokenName)
import Data.Newtype (class Newtype)
import JS.BigInt (BigInt)

-- | Parameters for treasury general and trip contracts
newtype TreasuryParams = TreasuryParams
  { configSymbol :: CurrencySymbol
  , configTokenName :: TokenName
  , tallySymbol :: CurrencySymbol
  , treasurySymbol :: CurrencySymbol
  }

derive instance Newtype TreasuryParams _

-- | Parameters for treasury fund contract
type TreasuryFundParams =
  { adaAmount :: BigInt
  , configSymbol :: CurrencySymbol
  , configTokenName :: TokenName
  }

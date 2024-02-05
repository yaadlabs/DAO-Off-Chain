{-|
Module: Dao.Component.Config.Params
Description: Helpers for create config workflow
-}
module Dao.Component.Config.Params (ConfigParams) where

import Contract.Value (CurrencySymbol, TokenName)
import JS.BigInt (BigInt)

-- | Parameters passed when initially creating dynamic config
type ConfigParams =
  { configTokenName :: TokenName
  , upgradeMajorityPercent :: BigInt
  , upgradeRelativeMajorityPercent :: BigInt
  , generalMajorityPercent :: BigInt
  , generalRelativeMajorityPercent :: BigInt
  , tripMajorityPercent :: BigInt
  , tripRelativeMajorityPercent :: BigInt
  , totalVotes :: BigInt
  , maxGeneralDisbursement :: BigInt
  , maxTripDisbursement :: BigInt
  , agentDisbursementPercent :: BigInt
  , proposalTallyEndOffset :: BigInt
  , tallyNft :: CurrencySymbol
  , voteCurrencySymbol :: CurrencySymbol
  , voteTokenName :: TokenName
  , voteNft :: CurrencySymbol
  , voteFungibleCurrencySymbol :: CurrencySymbol
  , voteFungibleTokenName :: TokenName
  , fungibleVotePercent :: BigInt
  -- Index needed for making tallyNft
  , indexSymbol :: CurrencySymbol
  , indexTokenName :: TokenName
  }

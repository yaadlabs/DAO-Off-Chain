{-|
Module: Dao.Component.Config.Params
Description: Config helpers
-}
module Dao.Component.Config.Params
  ( CreateConfigParams(..)
  , UpgradeConfigParams(..)
  , mkValidatorConfig
  ) where

import Contract.Value (CurrencySymbol, TokenName)
import Data.Newtype (class Newtype)
import JS.BigInt (BigInt)
import LambdaBuffers.ApplicationTypes.Configuration (DynamicConfigDatum)
import ScriptArguments.Types
  ( ValidatorParams(ValidatorParams)
  )

-- | Parameters passed when initially creating dynamic config
newtype CreateConfigParams = CreateConfigParams
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
  , voteTokenName :: TokenName
  -- VoteNft (vote pass) symbol
  , voteNftSymbol :: CurrencySymbol
  -- Fungible (vote multiplier) symbol
  , voteFungibleCurrencySymbol :: CurrencySymbol
  , voteFungibleTokenName :: TokenName
  , fungibleVotePercent :: BigInt
  -- Index needed for making tallyNft
  , indexSymbol :: CurrencySymbol
  , indexTokenName :: TokenName
  }

derive instance Newtype CreateConfigParams _

-- | Parameters passed for the upgrade config proposal contract
newtype UpgradeConfigParams = UpgradeConfigParams
  { newDynamicConfigDatum :: DynamicConfigDatum
  , configSymbol :: CurrencySymbol
  , configTokenName :: TokenName
  , tallySymbol :: CurrencySymbol
  , proposalTokenName :: TokenName
  }

derive instance Newtype UpgradeConfigParams _

mkValidatorConfig ::
  CurrencySymbol ->
  TokenName ->
  ValidatorParams
mkValidatorConfig symbol tokenName =
  ValidatorParams
    { vpConfigSymbol: symbol
    , vpConfigTokenName: tokenName
    }

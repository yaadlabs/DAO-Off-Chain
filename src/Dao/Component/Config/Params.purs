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
import LambdaBuffers.ApplicationTypes.Arguments
  ( ConfigurationValidatorConfig(ConfigurationValidatorConfig)
  )
import LambdaBuffers.ApplicationTypes.Configuration (DynamicConfigDatum)

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
  , tallyNft :: CurrencySymbol
  , voteCurrencySymbol :: CurrencySymbol
  , voteTokenName :: TokenName
  , voteNft :: CurrencySymbol
  , voteFungibleCurrencySymbol :: CurrencySymbol
  , voteFungibleTokenName :: TokenName
  , fungibleVotePercent :: BigInt
  }

derive instance Newtype CreateConfigParams _

-- | Parameters passed for the upgrade config proposal contract
newtype UpgradeConfigParams = UpgradeConfigParams
  { newDynamicConfigDatum :: DynamicConfigDatum
  , configSymbol :: CurrencySymbol
  , configTokenName :: TokenName
  , tallySymbol :: CurrencySymbol
  }

derive instance Newtype UpgradeConfigParams _

mkValidatorConfig ::
  CurrencySymbol ->
  TokenName ->
  ConfigurationValidatorConfig
mkValidatorConfig symbol tokenName =
  ConfigurationValidatorConfig
    { cvcConfigNftCurrencySymbol: symbol
    , cvcConfigNftTokenName: tokenName
    }

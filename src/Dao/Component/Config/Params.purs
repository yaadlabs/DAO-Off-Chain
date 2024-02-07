{-|
Module: Dao.Component.Config.Params
Description: Config helpers
-}
module Dao.Component.Config.Params
  ( ConfigParams
  , UpgradeConfigParams
  , mkValidatorConfig
  ) where

import Contract.Value (CurrencySymbol, TokenName)
import JS.BigInt (BigInt)
import LambdaBuffers.ApplicationTypes.Configuration (DynamicConfigDatum)
import ScriptArguments.Types
  ( ConfigurationValidatorConfig(ConfigurationValidatorConfig)
  )

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
  , voteTokenName :: TokenName
  , voteFungibleCurrencySymbol :: CurrencySymbol
  , voteFungibleTokenName :: TokenName
  , fungibleVotePercent :: BigInt
  -- Index needed for making tallyNft
  , indexSymbol :: CurrencySymbol
  , indexTokenName :: TokenName
  }

-- | Parameters passed for the upgrade config proposal contract
type UpgradeConfigParams =
  { newDynamicConfigDatum :: DynamicConfigDatum
  , configSymbol :: CurrencySymbol
  , configTokenName :: TokenName
  , tallySymbol :: CurrencySymbol
  }

mkValidatorConfig ::
  CurrencySymbol ->
  TokenName ->
  ConfigurationValidatorConfig
mkValidatorConfig symbol tokenName =
  ConfigurationValidatorConfig
    { cvcConfigNftCurrencySymbol: symbol
    , cvcConfigNftTokenName: tokenName
    }

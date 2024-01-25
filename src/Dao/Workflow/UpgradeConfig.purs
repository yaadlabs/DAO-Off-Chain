{-|
Module: Dao.Workflow.UpgradeConfig
Description: Contract for upgrading the dynamic config based on an upgrade proposal
-}
module Dao.Workflow.UpgradeConfig (upgradeConfig) where

import Contract.Log (logInfo')
import Contract.Monad (Contract)
import Contract.PlutusData (Datum(Datum), toData)
import Contract.Prelude
  ( bind
  , discard
  , mconcat
  , one
  , pure
  , unwrap
  , (#)
  , ($)
  , (*)
  , (+)
  , (/)
  , (>=)
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts (Validator, ValidatorHash, validatorHash)
import Contract.Time (POSIXTime(POSIXTime))
import Contract.Transaction
  ( TransactionHash
  , submitTxFromConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Value
  ( Value
  , adaToken
  , scriptCurrencySymbol
  , singleton
  )
import Dao.Component.Config.Params (UpgradeConfigParams, mkValidatorConfig)
import Dao.Component.Config.Query (ConfigInfo, spendConfigUtxo)
import Dao.Component.Tally.Query (TallyInfo, referenceTallyUtxo)
import Dao.Scripts.Policy.Upgrade (upgradePolicy)
import Dao.Scripts.Policy.Upgrade (upgradePolicy)
import Dao.Scripts.Validator.Config (unappliedConfigValidatorDebug)
import Dao.Scripts.Validator.Tally (unappliedTallyValidatorDebug)
import Dao.Utils.Error (guardContract)
import Dao.Utils.Time (mkOnchainTimeRange, mkValidityRange, oneMinute)
import JS.BigInt (BigInt, fromInt)
import LambdaBuffers.ApplicationTypes.Configuration (DynamicConfigDatum)
import LambdaBuffers.ApplicationTypes.Tally (TallyStateDatum)

-- | Contract for upgrading the dynamic config based on an upgrade proposal
upgradeConfig ::
  UpgradeConfigParams ->
  Contract TransactionHash
upgradeConfig params' =
  do
    logInfo' "Entering upgradeConfig transaction"

    let params = params' # unwrap

    -- Make the scripts
    let
      validatorConfig = mkValidatorConfig params.configSymbol
        params.configTokenName
    appliedTallyValidator :: Validator <- unappliedTallyValidatorDebug
      validatorConfig
    appliedConfigValidator :: Validator <- unappliedConfigValidatorDebug
      validatorConfig

    -- Query the UTXOs
    configInfo :: ConfigInfo <- spendConfigUtxo params.configSymbol
      appliedConfigValidator
    tallyInfo :: TallyInfo <- referenceTallyUtxo params.tallySymbol
      appliedTallyValidator

    -- Make on-chain time range
    timeRange <- mkValidityRange (POSIXTime $ fromInt $ 5 * oneMinute)
    onchainTimeRange <- mkOnchainTimeRange timeRange

    let
      newConfigDatum :: Datum
      newConfigDatum = Datum $ toData params.newDynamicConfigDatum

      configValidatorHash :: ValidatorHash
      configValidatorHash = validatorHash appliedConfigValidator

      oldDynamicConfig :: DynamicConfigDatum
      oldDynamicConfig = configInfo.datum

      tallyDatum :: TallyStateDatum
      tallyDatum = tallyInfo.datum

      votesFor :: BigInt
      votesFor = tallyDatum # unwrap # _.for

      votesAgainst :: BigInt
      votesAgainst = tallyDatum # unwrap # _.against

      totalVotes :: BigInt
      totalVotes = votesFor + votesAgainst

      configTotalVotes :: BigInt
      configTotalVotes = oldDynamicConfig # unwrap # _.totalVotes

      relativeMajority :: BigInt
      relativeMajority = (totalVotes * (fromInt 1000)) / configTotalVotes

      majorityPercent :: BigInt
      majorityPercent = (votesFor * (fromInt 1000)) / totalVotes

      configUpgradeRelativeMajorityPercent :: BigInt
      configUpgradeRelativeMajorityPercent = oldDynamicConfig # unwrap #
        _.upgradeRelativeMajorityPercent

      configUpgradeMajorityPercent :: BigInt
      configUpgradeMajorityPercent = oldDynamicConfig # unwrap #
        _.upgradeMajorityPercent

    -- Check for sufficient votes
    guardContract "Relative majority is too low" $ relativeMajority >=
      configUpgradeRelativeMajorityPercent
    guardContract "Majority percent is too low" $ majorityPercent >=
      configUpgradeMajorityPercent

    upgradePolicy' <- upgradePolicy

    let
      -- We use an always succeeds policy as a placeholder for this requirement
      upgradeToken :: Value
      upgradeToken = singleton (scriptCurrencySymbol upgradePolicy') adaToken
        one

      lookups :: Lookups.ScriptLookups
      lookups =
        mconcat
          [ configInfo.lookups
          , tallyInfo.lookups
          , Lookups.mintingPolicy upgradePolicy'
          ]

      constraints :: Constraints.TxConstraints
      constraints =
        mconcat
          [ Constraints.mustPayToScript
              configValidatorHash
              (Datum $ toData newConfigDatum)
              Constraints.DatumInline
              configInfo.value
          , Constraints.mustMintValue upgradeToken
          -- , Constraints.mustValidateIn onchainTimeRange
          , configInfo.constraints
          , tallyInfo.constraints
          ]

    txHash <- submitTxFromConstraints lookups constraints

    pure txHash

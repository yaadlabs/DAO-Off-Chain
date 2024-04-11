{-|
Module: Dao.Workflow.UpgradeConfig
Description: Contract for upgrading the dynamic config based on an upgrade proposal
-}
module Dao.Workflow.UpgradeConfig (upgradeConfig) where

import Contract.Chain (waitNSlots)
import Contract.Log (logInfo')
import Contract.Monad (Contract)
import Contract.Numeric.Natural (fromInt') as Natural
import Contract.PlutusData (Datum(Datum), toData)
import Contract.Prelude
  ( bind
  , discard
  , mconcat
  , one
  , pure
  , unwrap
  , void
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
import Dao.Workflow.ReferenceScripts (retrieveReferenceScript)
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

    -- We are updating the config datum so we need to spend the UTXO at
    -- the config validator holding the old datum, we will then create
    -- a new UTXO at the config validator holding the new config datum
    -- and also marked by the config NFT
    configValidatorRef <- retrieveReferenceScript $ unwrap
      appliedConfigValidator
    configInfo :: ConfigInfo <- spendConfigUtxo params.configSymbol
      appliedConfigValidator
      configValidatorRef
    tallyInfo :: TallyInfo <- referenceTallyUtxo params.tallySymbol
      params.proposalTokenName
      appliedTallyValidator

    -- Make on-chain time range
    timeRange <- mkValidityRange (POSIXTime $ fromInt $ 5 * oneMinute)
    onchainTimeRange <- mkOnchainTimeRange timeRange

    void $ waitNSlots (Natural.fromInt' 10)

    let
      -- The new config passed by the user to replace the old one
      newConfigDatum :: Datum
      newConfigDatum = Datum $ toData params.newDynamicConfigDatum

      -- The config that was held at the UTXO we are spending
      oldDynamicConfig :: DynamicConfigDatum
      oldDynamicConfig = configInfo.datum

      -- We need this to pay to the validator
      configValidatorHash :: ValidatorHash
      configValidatorHash = validatorHash appliedConfigValidator

      -- The tally datum holds the proposal related values
      -- We use these in our calculations
      tallyDatum :: TallyStateDatum
      tallyDatum = tallyInfo.datum

      -- Number of votes cast in favour of the proposal
      votesFor :: BigInt
      votesFor = tallyDatum # unwrap # _.for

      -- Number of votes cast in opposition to the proposal
      votesAgainst :: BigInt
      votesAgainst = tallyDatum # unwrap # _.against

      totalVotes :: BigInt
      totalVotes = votesFor + votesAgainst

      -- Must not be zero
      configTotalVotes :: BigInt
      configTotalVotes = oldDynamicConfig # unwrap # _.totalVotes

      -- Must exceed its corresponding config threshold
      relativeMajority :: BigInt
      relativeMajority = (totalVotes * (fromInt 1000)) / configTotalVotes

      -- Must exceed its corresponding config threshold
      majorityPercent :: BigInt
      majorityPercent = (votesFor * (fromInt 1000)) / totalVotes

      -- Vote threshold set at the config which must be exceeded
      -- in order for the effect to be executed
      configUpgradeRelativeMajorityPercent :: BigInt
      configUpgradeRelativeMajorityPercent = oldDynamicConfig # unwrap #
        _.upgradeRelativeMajorityPercent

      -- Vote threshold set at the config which must be exceeded
      configUpgradeMajorityPercent :: BigInt
      configUpgradeMajorityPercent = oldDynamicConfig # unwrap #
        _.upgradeMajorityPercent

    -- Check for sufficient votes
    guardContract "Relative majority is too low" $ relativeMajority >=
      configUpgradeRelativeMajorityPercent
    guardContract "Majority percent is too low" $ majorityPercent >=
      configUpgradeMajorityPercent

    -- The on-chain script requires an additional policy to which
    -- some of the validation of this tranasction will be offset to
    -- For now we use an always-succeeds policy as a placeholder
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
          -- We pay the new config passed by the user to a
          -- UTXO at the config validator marked by the config NFT
          -- This replaces the previous UTXO holding the datum which we just spent
          , Constraints.mustMintValue upgradeToken
          , Constraints.mustValidateIn onchainTimeRange
          -- ^ The script requires a time-range in order to ensure
          -- that the tallying period has passed
          , configInfo.constraints
          , tallyInfo.constraints
          ]

    txHash <- submitTxFromConstraints lookups constraints

    pure txHash

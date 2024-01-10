{-|
Module: Dao.Workflow.UpgradeConfig
Description: Contract for upgrading a the dynamic config based on an upgrade proposal
-}
module Dao.Workflow.UpgradeConfig (upgradeConfig) where

import Contract.Log (logInfo')
import Contract.Monad (Contract)
import Contract.PlutusData (Datum(Datum), toData)
import Contract.Prelude
  ( bind
  , discard
  , mconcat
  , pure
  , ($)
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts (Validator, ValidatorHash, validatorHash)
import Contract.Transaction
  ( TransactionHash
  , submitTxFromConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Value
  ( CurrencySymbol
  , TokenName
  )
import Dao.Component.Config.Params (mkValidatorConfig)
import Dao.Component.Config.Query (ConfigInfo, spendConfigUtxo)
import Dao.Component.Tally.Query (TallyInfo, referenceTallyUtxo)
import LambdaBuffers.ApplicationTypes.Configuration (DynamicConfigDatum)
import Scripts.ConfigValidator (unappliedConfigValidator)
import Scripts.TallyValidator (unappliedTallyValidator)

upgradeConfig ::
  CurrencySymbol ->
  CurrencySymbol ->
  TokenName ->
  DynamicConfigDatum ->
  Contract TransactionHash
upgradeConfig configSymbol tallySymbol configTokenName newDynamicConfigDatum =
  do
    logInfo' "Entering upgradeConfig transaction"

    -- Make the scripts
    let validatorConfig = mkValidatorConfig configSymbol configTokenName
    appliedTallyValidator :: Validator <- unappliedTallyValidator
      validatorConfig
    appliedConfigValidator :: Validator <- unappliedConfigValidator
      validatorConfig

    -- Query the UTXOs
    configInfo :: ConfigInfo <- spendConfigUtxo configSymbol
      appliedConfigValidator
    tallyInfo :: TallyInfo <- referenceTallyUtxo tallySymbol
      appliedTallyValidator

    let
      newConfigDatum :: Datum
      newConfigDatum = Datum $ toData newDynamicConfigDatum

      configValidatorHash :: ValidatorHash
      configValidatorHash = validatorHash appliedConfigValidator

      -- TODO: Need to include an upgrade policy in the lookups
      lookups :: Lookups.ScriptLookups
      lookups =
        mconcat
          [ configInfo.lookups
          , tallyInfo.lookups
          ]

      -- TODO: Need to include mustMintValue via an upgrade policy here too
      constraints :: Constraints.TxConstraints
      constraints =
        mconcat
          [ Constraints.mustPayToScript
              configValidatorHash
              (Datum $ toData newConfigDatum)
              Constraints.DatumInline
              configInfo.value
          , configInfo.constraints
          , tallyInfo.constraints
          ]

    txHash <- submitTxFromConstraints lookups constraints

    pure txHash

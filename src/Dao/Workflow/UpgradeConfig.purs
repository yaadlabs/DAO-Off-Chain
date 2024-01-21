{-|
Module: Dao.Workflow.UpgradeConfig
Description: Contract for upgrading a the dynamic config based on an upgrade proposal
-}
module Dao.Workflow.UpgradeConfig (upgradeConfig) where

import Contract.Address (scriptHashAddress)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedM)
import Contract.PlutusData (Datum(Datum), toData, unitRedeemer)
import Contract.Prelude
  ( type (/\)
  , bind
  , discard
  , mconcat
  , one
  , pure
  , ($)
  , (/\)
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts (Validator, ValidatorHash, validatorHash)
import Contract.Transaction
  ( TransactionHash
  , TransactionInput
  , TransactionOutputWithRefScript
  , submitTxFromConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Utxos (UtxoMap, utxosAt)
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , Value
  )
import Contract.Value (singleton) as Value
import Dao.Utils.Query (findUtxoByValue)
import Data.Map as Map
import Data.Maybe (Maybe(Nothing))
import LambdaBuffers.ApplicationTypes.Configuration (DynamicConfigDatum)
import ScriptArguments.Types
  ( ConfigurationValidatorConfig(ConfigurationValidatorConfig)
  )
import Scripts.ConfigValidator (unappliedConfigValidator)
import Scripts.TallyValidator (unappliedTallyValidator)

type TallyInfo = { tallySymbol :: CurrencySymbol, tallyTokenName :: TokenName }

upgradeConfig ::
  ConfigurationValidatorConfig ->
  TallyInfo ->
  DynamicConfigDatum ->
  Contract TransactionHash
upgradeConfig validatorConfig tallyInfo newDynamicConfigDatum = do
  logInfo' "Entering upgradeConfig transaction"

  appliedTallyValidator :: Validator <- unappliedTallyValidator validatorConfig
  appliedConfigValidator :: Validator <- unappliedConfigValidator
    validatorConfig

  let
    configValidatorAddress = scriptHashAddress
      (validatorHash appliedConfigValidator)
      Nothing
  configValidatorUtxoMap <- utxosAt configValidatorAddress

  let
    tallyValidatorAddress = scriptHashAddress
      (validatorHash appliedTallyValidator)
      Nothing
  tallyValidatorUtxoMap <- utxosAt tallyValidatorAddress

  (configUtxoTxInput /\ configUtxoTxOutRefScript) <-
    liftedM "Could not find config UTXO" $ getConfigUtxo validatorConfig
      configValidatorUtxoMap
  (tallyUtxoTxInput /\ _) <-
    liftedM "Could not find tally UTXO" $ getTallyUtxo tallyInfo
      tallyValidatorUtxoMap

  let
    newConfigDatum :: Datum
    newConfigDatum = Datum $ toData newDynamicConfigDatum

    configNft :: Value
    configNft =
      let
        ( ConfigurationValidatorConfig
            { cvcConfigNftCurrencySymbol, cvcConfigNftTokenName }
        ) = validatorConfig
      in
        (Value.singleton cvcConfigNftCurrencySymbol cvcConfigNftTokenName one)

    configValidatorHash :: ValidatorHash
    configValidatorHash = validatorHash appliedConfigValidator

    -- TODO: Need to include an upgrade policy in the lookups
    lookups :: Lookups.ScriptLookups
    lookups =
      mconcat
        [ Lookups.unspentOutputs $ Map.singleton configUtxoTxInput
            configUtxoTxOutRefScript
        ]

    -- TODO: Need to include mustMintValue via an upgrade policy here too
    constraints :: Constraints.TxConstraints
    constraints =
      mconcat
        [ Constraints.mustPayToScript
            configValidatorHash
            (Datum $ toData newConfigDatum)
            Constraints.DatumInline
            configNft
        , Constraints.mustSpendScriptOutput configUtxoTxInput unitRedeemer
        , Constraints.mustReferenceOutput tallyUtxoTxInput
        ]

  txHash <- submitTxFromConstraints lookups constraints

  pure txHash
  where
  getConfigUtxo ::
    ConfigurationValidatorConfig ->
    UtxoMap ->
    Contract (Maybe (TransactionInput /\ TransactionOutputWithRefScript))
  getConfigUtxo
    ( ConfigurationValidatorConfig
        { cvcConfigNftCurrencySymbol, cvcConfigNftTokenName }
    ) =
    findUtxoByValue
      (Value.singleton cvcConfigNftCurrencySymbol cvcConfigNftTokenName one)

  getTallyUtxo ::
    TallyInfo ->
    UtxoMap ->
    Contract (Maybe (TransactionInput /\ TransactionOutputWithRefScript))
  getTallyUtxo ({ tallySymbol, tallyTokenName }) =
    findUtxoByValue
      (Value.singleton tallySymbol tallyTokenName one)

{-|
Module: Dao.Workflow.CreateConfig
Description: Contract for creating dynamic config datum
  and locking it at UTXO at config validator marked by config NFT
-}
module Dao.Workflow.CreateConfig (createConfig) where

import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData (Datum(Datum), toData)
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
import Contract.Scripts (MintingPolicy, Validator, ValidatorHash, validatorHash)
import Contract.Transaction
  ( TransactionHash
  , TransactionInput
  , TransactionOutputWithRefScript
  , submitTxFromConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , Value
  , scriptCurrencySymbol
  )
import Contract.Value (singleton) as Value
import Dao.Utils.Query (getAllWalletUtxos)
import Data.Array (head)
import Data.Map as Map
import LambdaBuffers.ApplicationTypes.Configuration (DynamicConfigDatum)
import ScriptArguments.Types
  ( ConfigurationValidatorConfig(ConfigurationValidatorConfig)
  , NftConfig(NftConfig)
  )
import Scripts.ConfigPolicy (unappliedConfigPolicy)
import Scripts.ConfigValidator (unappliedConfigValidator)

createConfig ::
  DynamicConfigDatum ->
  TokenName ->
  Contract (TransactionHash /\ CurrencySymbol)
createConfig dynamicConfig configTokenName = do
  logInfo' "Entering createConfig transaction"

  userUtxos <- getAllWalletUtxos

  configSpend <- liftContractM "No UTXOs found"
    $ head
    $ Map.toUnfoldable userUtxos

  dynamicConfigInfo <-
    buildDynamicConfig
      dynamicConfig
      configTokenName
      configSpend

  let
    lookups :: Lookups.ScriptLookups
    lookups = dynamicConfigInfo.lookups

    constraints :: Constraints.TxConstraints
    constraints = dynamicConfigInfo.constraints

  txHash <- submitTxFromConstraints lookups constraints

  pure (txHash /\ dynamicConfigInfo.symbol)

type ConfigInfo =
  { symbol :: CurrencySymbol
  , lookups :: Lookups.ScriptLookups
  , constraints :: Constraints.TxConstraints
  }

buildDynamicConfig ::
  DynamicConfigDatum ->
  TokenName ->
  (TransactionInput /\ TransactionOutputWithRefScript) ->
  Contract ConfigInfo
buildDynamicConfig dynamicConfig configTokenName (txInput /\ txInputWithScript) =
  do
    logInfo' "Entering buildDynamicConfig transaction"

    let
      configPolicyParams :: NftConfig
      configPolicyParams = NftConfig
        { ncInitialUtxo: txInput, ncTokenName: configTokenName }

    appliedConfigPolicy :: MintingPolicy <- unappliedConfigPolicy
      configPolicyParams

    let
      configSymbol :: CurrencySymbol
      configSymbol = scriptCurrencySymbol appliedConfigPolicy

      configValidatorParams :: ConfigurationValidatorConfig
      configValidatorParams =
        ConfigurationValidatorConfig
          { cvcConfigNftCurrencySymbol: configSymbol
          , cvcConfigNftTokenName: configTokenName
          }

    appliedConfigValidator :: Validator <- unappliedConfigValidator
      configValidatorParams

    let
      configValidatorHash :: ValidatorHash
      configValidatorHash = validatorHash appliedConfigValidator

      nftConfig :: Value
      nftConfig = Value.singleton configSymbol configTokenName one

      configDatum :: Datum
      configDatum = Datum $ toData dynamicConfig

      lookups' :: Lookups.ScriptLookups
      lookups' = mconcat
        [ Lookups.mintingPolicy appliedConfigPolicy
        , Lookups.unspentOutputs $ Map.singleton txInput txInputWithScript
        ]

      constraints' :: Constraints.TxConstraints
      constraints' = mconcat
        [ Constraints.mustMintValue nftConfig
        , Constraints.mustSpendPubKeyOutput txInput
        , Constraints.mustPayToScript
            configValidatorHash
            configDatum
            Constraints.DatumInline
            nftConfig
        ]

    pure { symbol: configSymbol, lookups: lookups', constraints: constraints' }

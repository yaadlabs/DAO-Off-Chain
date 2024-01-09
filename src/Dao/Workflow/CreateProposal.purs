{-|
Module: Dao.Workflow.CreateProposal
Description: Contract for creating a proposal
-}
module Dao.Workflow.CreateProposal (createProposal) where

import Contract.Address (scriptHashAddress)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.PlutusData (Datum(Datum), fromData, toData, unitRedeemer)
import Contract.Prelude
  ( type (/\)
  , Unit
  , bind
  , discard
  , mconcat
  , one
  , pure
  , show
  , unit
  , (#)
  , ($)
  , (+)
  , (/\)
  , (<>)
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
import Contract.Utxos (UtxoMap, utxosAt)
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , Value
  , scriptCurrencySymbol
  )
import Contract.Value (singleton) as Value
import Dao.Component.Index.Query (IndexInfo, getIndexInfo)
import Dao.Utils.Datum
  ( getInlineDatumFromTxOutWithRefScript
  )
import Dao.Utils.Query
  ( QueryType(Spend, Reference)
  , UtxoInfo
  , findUtxoBySymbol
  , findUtxoByValue
  )
import Dao.Utils.Value (mkTokenName)
import Data.Map as Map
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (unwrap)
import JS.BigInt (fromInt)
import LambdaBuffers.ApplicationTypes.Arguments
  ( ConfigurationValidatorConfig(ConfigurationValidatorConfig)
  )
import LambdaBuffers.ApplicationTypes.Configuration (DynamicConfigDatum)
import LambdaBuffers.ApplicationTypes.Index (IndexNftDatum(IndexNftDatum))
import LambdaBuffers.ApplicationTypes.Tally (TallyStateDatum)
import ScriptArguments.Types
  ( -- ConfigurationValidatorConfig(ConfigurationValidatorConfig)
    TallyNftConfig(TallyNftConfig)
  )
import Scripts.ConfigValidator
  ( unappliedConfigValidator
  , unappliedConfigValidatorDebug
  )
import Scripts.IndexValidator (indexValidatorScript, indexValidatorScriptDebug)
import Scripts.TallyPolicy (unappliedTallyPolicy, unappliedTallyPolicyDebug)
import Scripts.TallyValidator (unappliedTallyValidator)
import Type.Proxy (Proxy(Proxy))

createProposal ::
  CurrencySymbol ->
  CurrencySymbol ->
  TokenName ->
  TokenName ->
  TallyStateDatum ->
  Contract (TransactionHash /\ CurrencySymbol)
createProposal
  configSymbol
  indexSymbol
  configTokenName
  indexTokenName
  tallyStateDatum = do
  logInfo' "Entering createProposal transaction"

  let validatorConfig = mkValidatorConfig configSymbol configTokenName
  appliedTallyValidator :: Validator <- unappliedTallyValidator validatorConfig
  appliedConfigValidator :: Validator <- unappliedConfigValidatorDebug
    validatorConfig
  indexValidator :: Validator <- indexValidatorScriptDebug

  configInfo <- findUtxoBySymbol (Proxy :: Proxy DynamicConfigDatum) Reference
    configSymbol
    appliedConfigValidator
  indexInfo <- findUtxoBySymbol (Proxy :: Proxy IndexNftDatum) Spend indexSymbol
    indexValidator
  indexInfo' :: IndexInfo <- getIndexInfo indexSymbol

  logInfo' $ "configInfo: " <> show configInfo
  logInfo' $ "indexInfo: " <> show indexInfo
  logInfo' $ "indexInfo'" <> show indexInfo'

  let
    tallyConfig = mkTallyConfig configSymbol indexSymbol configTokenName
      indexTokenName
  appliedTallyPolicy :: MintingPolicy <- unappliedTallyPolicyDebug tallyConfig

  let
    updatedIndexDatum :: IndexNftDatum
    updatedIndexDatum = incrementIndexDatum indexInfo.datum

  tallyTokenName :: TokenName <-
    liftContractM "Could not make tally token name" $
      mkTallyTokenName updatedIndexDatum

  let
    tallySymbol :: CurrencySymbol
    tallySymbol = scriptCurrencySymbol appliedTallyPolicy

    indexValidatorHash :: ValidatorHash
    indexValidatorHash = validatorHash indexValidator

    tallyValidatorHash :: ValidatorHash
    tallyValidatorHash = validatorHash appliedTallyValidator

    tallyNft :: Value
    tallyNft = Value.singleton tallySymbol tallyTokenName one

    lookups :: Lookups.ScriptLookups
    lookups =
      mconcat
        [ Lookups.mintingPolicy appliedTallyPolicy
        , Lookups.validator indexValidator
        , indexInfo.lookups
        , configInfo.lookups
        ]

    constraints :: Constraints.TxConstraints
    constraints =
      mconcat
        [ Constraints.mustMintValue tallyNft
        , Constraints.mustPayToScript
            tallyValidatorHash
            (Datum $ toData tallyStateDatum)
            Constraints.DatumInline
            tallyNft
        , Constraints.mustPayToScript
            indexValidatorHash
            (Datum $ toData updatedIndexDatum)
            Constraints.DatumInline
            indexInfo.value
        , configInfo.constraints
        , indexInfo.constraints
        ]

  txHash <- submitTxFromConstraints lookups constraints

  pure (txHash /\ tallySymbol)
  where
  incrementIndexDatum :: IndexNftDatum -> IndexNftDatum
  incrementIndexDatum (IndexNftDatum { index: oldIndex }) =
    IndexNftDatum { index: oldIndex + (fromInt 1) }

  mkTallyTokenName :: IndexNftDatum -> Maybe TokenName
  mkTallyTokenName (IndexNftDatum index) = mkTokenName $ show index

  mkValidatorConfig ::
    CurrencySymbol -> TokenName -> ConfigurationValidatorConfig
  mkValidatorConfig configSymbol configTokenName =
    ConfigurationValidatorConfig
      { cvcConfigNftCurrencySymbol: configSymbol
      , cvcConfigNftTokenName: configTokenName
      }

  mkTallyConfig ::
    CurrencySymbol -> CurrencySymbol -> TokenName -> TokenName -> TallyNftConfig
  mkTallyConfig configSymbol indexSymbol configTokenName indexTokenName =
    TallyNftConfig
      { tncConfigNftCurrencySymbol: configSymbol
      , tncConfigNftTokenName: configTokenName
      , tncIndexNftPolicyId: indexSymbol
      , tncIndexNftTokenName: indexTokenName
      }

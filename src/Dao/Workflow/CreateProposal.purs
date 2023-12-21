{-|
Module: Dao.Workflow.CreateProposal
Description: Contract for creating a proposal
-}
module Dao.Workflow.CreateProposal (createProposal) where

import Contract.Prelude

import Contract.Address (scriptHashAddress)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.PlutusData (Datum(Datum), fromData, toData, unitRedeemer)
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
import Dao.Utils.Datum
  ( getInlineDatumFromTxOutWithRefScript
  )
import Dao.Utils.Query (findUtxoByValue)
import Dao.Utils.Value (mkTokenName)
import Data.Map as Map
import JS.BigInt (fromInt)
import LambdaBuffers.ApplicationTypes.Index (IndexNftDatum(IndexNftDatum))
import LambdaBuffers.ApplicationTypes.Tally (TallyStateDatum)
import ScriptArguments.Types
  ( ConfigurationValidatorConfig(ConfigurationValidatorConfig)
  , TallyNftConfig(TallyNftConfig)
  )
import Scripts.ConfigValidator (unappliedConfigValidator)
import Scripts.IndexValidator (indexValidatorScript)
import Scripts.TallyPolicy (unappliedTallyPolicy)
import Scripts.TallyValidator (unappliedTallyValidator)

createProposal ::
  TallyNftConfig ->
  TallyStateDatum ->
  Contract (TransactionHash /\ CurrencySymbol)
createProposal tallyConfig tallyStateDatum = do
  logInfo' "Entering createProposal transaction"

  let validatorConfig = mkValidatorConfig tallyConfig
  appliedTallyValidator :: Validator <- unappliedTallyValidator validatorConfig
  appliedConfigValidator :: Validator <- unappliedConfigValidator
    validatorConfig

  let
    configValidatorAddress = scriptHashAddress
      (validatorHash appliedConfigValidator)
      Nothing
  configValidatorUtxoMap <- utxosAt configValidatorAddress

  let
    indexValidatorAddress = scriptHashAddress
      (validatorHash indexValidatorScript)
      Nothing
  indexValidatorUtxoMap <- utxosAt indexValidatorAddress

  (configUtxoTxInput /\ _) <-
    liftedM "Could not find config UTXO" $ getConfigUtxo tallyConfig
      configValidatorUtxoMap
  (indexUtxoTxInput /\ indexUtxoTxOutRefScript) <-
    liftedM "Could not find index UTXO" $ getIndexUtxo tallyConfig
      indexValidatorUtxoMap

  oldIndexDatum' :: Datum <-
    liftContractM "No Inline index datum at OutputDatum" $
      getInlineDatumFromTxOutWithRefScript indexUtxoTxOutRefScript
  oldIndexDatum :: IndexNftDatum <-
    liftContractM "Could not convert datum" $ fromData (unwrap oldIndexDatum')

  appliedTallyPolicy :: MintingPolicy <- unappliedTallyPolicy tallyConfig

  let
    updatedIndexDatum :: IndexNftDatum
    updatedIndexDatum = incrementIndexDatum oldIndexDatum

  tallyTokenName :: TokenName <-
    liftContractM "Could not make tally token name" $
      mkTallyTokenName updatedIndexDatum

  let
    tallySymbol :: CurrencySymbol
    tallySymbol = scriptCurrencySymbol appliedTallyPolicy

    indexValidatorHash :: ValidatorHash
    indexValidatorHash = validatorHash indexValidatorScript

    tallyValidatorHash :: ValidatorHash
    tallyValidatorHash = validatorHash appliedTallyValidator

    tallyNft :: Value
    tallyNft = Value.singleton tallySymbol tallyTokenName one

    indexNft :: Value
    indexNft =
      let
        txOut = (unwrap indexUtxoTxOutRefScript).output
      in
        (unwrap txOut).amount

    lookups :: Lookups.ScriptLookups
    lookups =
      mconcat
        [ Lookups.mintingPolicy appliedTallyPolicy
        , Lookups.unspentOutputs $ Map.singleton indexUtxoTxInput
            indexUtxoTxOutRefScript
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
        , Constraints.mustSpendScriptOutput indexUtxoTxInput unitRedeemer
        , Constraints.mustPayToScript
            indexValidatorHash
            (Datum $ toData updatedIndexDatum)
            Constraints.DatumInline
            indexNft
        , Constraints.mustReferenceOutput configUtxoTxInput
        ]

  txHash <- submitTxFromConstraints lookups constraints

  pure (txHash /\ tallySymbol)
  where
  incrementIndexDatum :: IndexNftDatum -> IndexNftDatum
  incrementIndexDatum (IndexNftDatum { index: oldIndex }) =
    IndexNftDatum { index: oldIndex + (fromInt 1) }

  mkTallyTokenName :: IndexNftDatum -> Maybe TokenName
  mkTallyTokenName (IndexNftDatum index) = mkTokenName $ show index

  mkValidatorConfig :: TallyNftConfig -> ConfigurationValidatorConfig
  mkValidatorConfig
    (TallyNftConfig { tncConfigNftCurrencySymbol, tncConfigNftTokenName }) =
    ConfigurationValidatorConfig
      { cvcConfigNftCurrencySymbol: tncConfigNftCurrencySymbol
      , cvcConfigNftTokenName: tncConfigNftTokenName
      }

  getConfigUtxo ::
    TallyNftConfig ->
    UtxoMap ->
    Contract (Maybe (TransactionInput /\ TransactionOutputWithRefScript))
  getConfigUtxo
    (TallyNftConfig { tncConfigNftCurrencySymbol, tncConfigNftTokenName }) =
    findUtxoByValue
      (Value.singleton tncConfigNftCurrencySymbol tncConfigNftTokenName one)

  getIndexUtxo ::
    TallyNftConfig ->
    UtxoMap ->
    Contract (Maybe (TransactionInput /\ TransactionOutputWithRefScript))
  getIndexUtxo (TallyNftConfig { tncIndexNftPolicyId, tncIndexNftTokenName }) =
    findUtxoByValue
      (Value.singleton tncIndexNftPolicyId tncIndexNftTokenName one)

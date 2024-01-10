{-|
Module: Dao.Workflow.CreateProposal
Description: Contract for creating a proposal
-}
module Dao.Workflow.CreateProposal (createProposal) where

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
  , show
  , ($)
  , (+)
  , (/\)
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy, Validator, ValidatorHash, validatorHash)
import Contract.Transaction
  ( TransactionHash
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
import Dao.Component.Config.Query (ConfigInfo, referenceConfigUtxo)
import Dao.Component.Index.Query (IndexInfo, spendIndexUtxo)
import Dao.Utils.Value (mkTokenName)
import Data.Maybe (Maybe)
import JS.BigInt (fromInt)
import LambdaBuffers.ApplicationTypes.Arguments
  ( ConfigurationValidatorConfig(ConfigurationValidatorConfig)
  )
import LambdaBuffers.ApplicationTypes.Index (IndexNftDatum(IndexNftDatum))
import LambdaBuffers.ApplicationTypes.Tally (TallyStateDatum)
import ScriptArguments.Types (TallyNftConfig(TallyNftConfig))
import Scripts.ConfigValidator (unappliedConfigValidatorDebug)
import Scripts.IndexValidator (indexValidatorScriptDebug)
import Scripts.TallyPolicy (unappliedTallyPolicyDebug)
import Scripts.TallyValidator (unappliedTallyValidator)

-- | Contract for creating a new proposal
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

  -- Make the scripts
  let
    validatorConfig = mkValidatorConfig configSymbol configTokenName
    tallyConfig = mkTallyConfig configSymbol indexSymbol configTokenName
      indexTokenName

  appliedTallyPolicy :: MintingPolicy <- unappliedTallyPolicyDebug tallyConfig
  appliedTallyValidator :: Validator <- unappliedTallyValidator validatorConfig
  appliedConfigValidator :: Validator <- unappliedConfigValidatorDebug
    validatorConfig
  indexValidator :: Validator <- indexValidatorScriptDebug

  -- Query the UTXOs
  configInfo :: ConfigInfo <- referenceConfigUtxo configSymbol
    appliedConfigValidator
  indexInfo :: IndexInfo <- spendIndexUtxo indexSymbol indexValidator

  let
    -- The index field of the IndexDatum must be incremented
    -- by one for each new proposal created
    updatedIndexDatum :: IndexNftDatum
    updatedIndexDatum = incrementIndexDatum indexInfo.datum

  -- The tally token name corresponds to the index field of the index datum
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
  mkValidatorConfig symbol tokenName =
    ConfigurationValidatorConfig
      { cvcConfigNftCurrencySymbol: symbol
      , cvcConfigNftTokenName: tokenName
      }

  mkTallyConfig ::
    CurrencySymbol -> CurrencySymbol -> TokenName -> TokenName -> TallyNftConfig
  mkTallyConfig configSymbol' indexSymbol' configTokenName' indexTokenName' =
    TallyNftConfig
      { tncConfigNftCurrencySymbol: configSymbol'
      , tncConfigNftTokenName: configTokenName'
      , tncIndexNftPolicyId: indexSymbol'
      , tncIndexNftTokenName: indexTokenName'
      }

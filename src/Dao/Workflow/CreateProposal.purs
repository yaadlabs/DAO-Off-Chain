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
  , unwrap
  , (#)
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
import Dao.Component.Config.Params (mkValidatorConfig)
import Dao.Component.Config.Query (ConfigInfo, referenceConfigUtxo)
import Dao.Component.Index.Query (IndexInfo, spendIndexUtxo)
import Dao.Component.Proposal.Params (CreateProposalParams)
import Dao.Scripts.Policy.TallyPolicy (unappliedTallyPolicyDebug)
import Dao.Scripts.Validator.ConfigValidator (unappliedConfigValidatorDebug)
import Dao.Scripts.Validator.IndexValidator (indexValidatorScriptDebug)
import Dao.Scripts.Validator.TallyValidator (unappliedTallyValidator)
import Dao.Utils.Contract (ContractResult(ContractResult))
import Dao.Utils.Value (mkTokenName)
import Data.Maybe (Maybe)
import JS.BigInt (fromInt)
import LambdaBuffers.ApplicationTypes.Arguments
  ( ConfigurationValidatorConfig(ConfigurationValidatorConfig)
  )
import LambdaBuffers.ApplicationTypes.Index (IndexNftDatum(IndexNftDatum))
import LambdaBuffers.ApplicationTypes.Tally (TallyStateDatum)
import ScriptArguments.Types (TallyNftConfig(TallyNftConfig))

-- | Contract for creating a proposal
createProposal ::
  CreateProposalParams ->
  Contract ContractResult
createProposal params' = do
  logInfo' "Entering createProposal transaction"

  let params = params' # unwrap

  let
    validatorConfig = mkValidatorConfig params.configSymbol
      params.configTokenName
  appliedTallyValidator :: Validator <- unappliedTallyValidator validatorConfig
  appliedConfigValidator :: Validator <- unappliedConfigValidatorDebug
    validatorConfig
  indexValidator :: Validator <- indexValidatorScriptDebug

  -- Query the UTXOs
  configInfo :: ConfigInfo <- referenceConfigUtxo params.configSymbol
    appliedConfigValidator
  indexInfo :: IndexInfo <- spendIndexUtxo params.indexSymbol
    indexValidator

  let
    tallyConfig = mkTallyConfig params.configSymbol
      params.indexSymbol
      params.configTokenName
      params.indexTokenName
  appliedTallyPolicy :: MintingPolicy <- unappliedTallyPolicyDebug tallyConfig

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
            (Datum $ toData $ params.tallyStateDatum)
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

  pure $ ContractResult
    { txHash, symbol: tallySymbol, tokenName: tallyTokenName }
  where
  incrementIndexDatum :: IndexNftDatum -> IndexNftDatum
  incrementIndexDatum (IndexNftDatum { index: oldIndex }) =
    IndexNftDatum { index: oldIndex + (fromInt 1) }

  mkTallyTokenName :: IndexNftDatum -> Maybe TokenName
  mkTallyTokenName (IndexNftDatum index) = mkTokenName $ show index

  mkTallyConfig ::
    CurrencySymbol -> CurrencySymbol -> TokenName -> TokenName -> TallyNftConfig
  mkTallyConfig configSymbol' indexSymbol' configTokenName' indexTokenName' =
    TallyNftConfig
      { tncConfigNftCurrencySymbol: configSymbol'
      , tncConfigNftTokenName: configTokenName'
      , tncIndexNftPolicyId: indexSymbol'
      , tncIndexNftTokenName: indexTokenName'
      }

{-|
Module: Dao.Workflow.CreateProposal
Description: Contract for creating a proposal
-}
module Dao.Workflow.CreateProposal (createProposal) where

import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.PlutusData (Datum(Datum), fromData, toData, unitRedeemer)
import Contract.Prelude
  ( type (/\)
  , bind
  , discard
  , mconcat
  , one
  , pure
  , show
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
import Dao.Component.Config.Query (ConfigInfo, getConfigInfo)
import Dao.Component.Index.Query (IndexInfo, getIndexInfo)
import Dao.Component.Proposal.Params (CreateProposalParams)
import Dao.Utils.Datum
  ( getInlineDatumFromTxOutWithRefScript
  )
import Dao.Utils.Value (mkTokenName)
import Data.Map as Map
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (unwrap)
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
import Scripts.TallyValidator (unappliedTallyValidatorDebug)

-- | Contract for creating a proposal
createProposal ::
  CreateProposalParams ->
  TallyStateDatum ->
  Contract (TransactionHash /\ CurrencySymbol)
createProposal
  proposalParams
  tallyStateDatum = do
  logInfo' "Entering createProposal transaction"

  let
    validatorConfig = mkValidatorConfig proposalParams.configSymbol
      proposalParams.configTokenName
  appliedTallyValidator :: Validator <- unappliedTallyValidatorDebug
    validatorConfig
  appliedConfigValidator :: Validator <- unappliedConfigValidatorDebug
    validatorConfig
  indexValidator :: Validator <- indexValidatorScriptDebug

  configInfo :: ConfigInfo <- getConfigInfo proposalParams.configSymbol
    appliedConfigValidator
  indexInfo :: IndexInfo <- getIndexInfo proposalParams.indexSymbol

  let
    tallyConfig = mkTallyConfig proposalParams.configSymbol
      proposalParams.indexSymbol
      proposalParams.configTokenName
      proposalParams.indexTokenName
  appliedTallyPolicy :: MintingPolicy <- unappliedTallyPolicyDebug tallyConfig

  let
    updatedIndexDatum :: IndexNftDatum
    updatedIndexDatum = incrementIndexDatum indexInfo.indexDatum

  tallyTokenName :: TokenName <-
    liftContractM "Could not make tally token name" $
      mkTallyTokenName indexInfo.indexDatum

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
            indexInfo.indexValue
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
  mkTallyTokenName indexDatum =
    mkTokenName $ show $ indexDatum # unwrap # _.index

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
      { tncIndexNftPolicyId: indexSymbol
      , tncConfigNftTokenName: configTokenName
      , tncConfigNftCurrencySymbol: configSymbol
      , tncIndexNftTokenName: indexTokenName
      }

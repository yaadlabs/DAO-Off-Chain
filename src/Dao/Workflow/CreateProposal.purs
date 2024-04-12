{-|
Module: Dao.Workflow.CreateProposal
Description: Contract for creating a proposal
-}
module Dao.Workflow.CreateProposal (createProposal) where

import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData (Datum(Datum), toData)
import Contract.Prelude
  ( bind
  , discard
  , mconcat
  , one
  , pure
  , show
  , (#)
  , ($)
  , (+)
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts
  ( MintingPolicy
  , Validator
  , ValidatorHash(ValidatorHash)
  , validatorHash
  )
import Contract.Transaction (submitTxFromConstraints)
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
import Dao.Component.Tally.Params (mkTallyConfig)
import Dao.Scripts.Policy (unappliedTallyPolicy)
import Dao.Scripts.Validator
  ( unappliedConfigValidator
  , indexValidatorScript
  )
import Dao.Utils.Contract (ContractResult(ContractResult))
import Dao.Utils.Value (mkTokenName)
import Dao.Workflow.ReferenceScripts (retrieveReferenceScript)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import JS.BigInt (fromInt)
import LambdaBuffers.ApplicationTypes.Configuration (DynamicConfigDatum)
import LambdaBuffers.ApplicationTypes.Index (IndexDatum(IndexDatum))

-- | Contract for creating a proposal
createProposal ::
  CreateProposalParams ->
  Contract ContractResult
createProposal params' = do
  logInfo' "Entering createProposal transaction"

  let params = params' # unwrap

  -- Make the scripts
  let
    validatorConfig = mkValidatorConfig params.configSymbol
      params.configTokenName
  appliedConfigValidator :: Validator <- unappliedConfigValidator
    validatorConfig
  indexValidator :: Validator <- indexValidatorScript

  -- Query the UTXOs
  configInfo :: ConfigInfo <- referenceConfigUtxo params.configSymbol
    appliedConfigValidator
  indexInfo :: IndexInfo <- spendIndexUtxo params.indexSymbol
    indexValidator

  -- Make the tally policy script
  let
    tallyConfig = mkTallyConfig params.configSymbol
      params.indexSymbol
      params.configTokenName
      params.indexTokenName
  appliedTallyPolicy :: MintingPolicy <- unappliedTallyPolicy tallyConfig

  let
    -- The index field of the IndexDatum must be incremented
    -- by one for each new proposal created
    updatedIndexDatum :: IndexDatum
    updatedIndexDatum = incrementIndexDatum indexInfo.datum

  -- The tally token name corresponds to the index field of the index datum
  -- Hence we use that to make the token here
  tallyTokenName :: TokenName <-
    liftContractM "Could not make tally token name" $
      mkTallyTokenName indexInfo.datum

  let
    -- The main config referenced at the config UTXO
    configDatum :: DynamicConfigDatum
    configDatum = configInfo.datum

    -- We need to send the tally datum to the tally validator
    tallyValidatorHash :: ValidatorHash
    tallyValidatorHash = ValidatorHash $ configDatum # unwrap # _.tallyValidator

    tallySymbol :: CurrencySymbol
    tallySymbol = scriptCurrencySymbol appliedTallyPolicy

    -- The token that will mark the UTXO at the tally validator
    -- containing the new tally datum passed by the user
    tallyNft :: Value
    tallyNft = Value.singleton tallySymbol tallyTokenName one

    -- We need to send the updated index datum to the index validator
    indexValidatorHash :: ValidatorHash
    indexValidatorHash = validatorHash indexValidator

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
            (Datum $ toData params.tallyStateDatum)
            Constraints.DatumInline
            tallyNft
        -- ^ We pay the newly created tally datum (passed as an argument by the user)
        -- to a UTXO at the tally validator, marked by the 'tallyNft'
        , Constraints.mustPayToScript
            indexValidatorHash
            (Datum $ toData updatedIndexDatum)
            Constraints.DatumInline
            indexInfo.value
        -- ^ We pay the updated index datum (with its index incremented)
        -- back to the index validator
        , configInfo.constraints
        , indexInfo.constraints
        ]

  txHash <- submitTxFromConstraints lookups constraints

  pure $ ContractResult
    { txHash, symbol: tallySymbol, tokenName: tallyTokenName }
  where
  -- The index must be incremented by one for each new proposal
  incrementIndexDatum :: IndexDatum -> IndexDatum
  incrementIndexDatum (IndexDatum { index: oldIndex }) =
    IndexDatum { index: oldIndex + (fromInt 1) }

  -- The tally token name corresponds to the
  -- 'index' field of the index datum
  mkTallyTokenName :: IndexDatum -> Maybe TokenName
  mkTallyTokenName indexDatum =
    mkTokenName $ show $ indexDatum # unwrap # _.index

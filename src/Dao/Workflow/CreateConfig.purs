{-|
Module: Dao.Workflow.CreateConfig
Description: Contract for creating dynamic config datum
  and locking it at UTXO at config validator marked by config NFT
-}
module Dao.Workflow.CreateConfig
  ( CreateConfigResult(CreateConfigResult)
  , createConfig
  ) where

import Cardano.ToData (toData)
import Cardano.Types
  ( AssetName
  , PlutusData
  , PlutusScript
  , ScriptHash
  , TransactionHash
  , TransactionInput
  , TransactionOutput
  , Value
  )
import Cardano.Types.BigNum (one) as BigNum
import Cardano.Types.Mint (fromMultiAsset) as Mint
import Cardano.Types.PlutusScript (hash) as PlutusScript
import Cardano.Types.Value (getMultiAsset, singleton) as Value
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.Prelude
  ( type (/\)
  , bind
  , discard
  , mconcat
  , pure
  , unwrap
  , (#)
  , ($)
  , (/\)
  )
import Contract.ScriptLookups as Lookups
import Contract.Transaction (submitTxFromConstraints)
import Contract.TxConstraints as Constraints
import Dao.Component.Config.Params (CreateConfigParams)
import Dao.Component.Tally.Params (mkTallyConfig)
import Dao.Scripts.Policy
  ( unappliedConfigPolicy
  , unappliedTallyPolicy
  , unappliedVotePolicy
  , voteNftPolicy
  )
import Dao.Scripts.Validator
  ( unappliedConfigValidator
  , unappliedTallyValidator
  , unappliedTreasuryValidator
  , unappliedVoteValidator
  )
import Dao.Utils.Query (getAllWalletUtxos)
import Data.Array (head)
import Data.Map as Map
import LambdaBuffers.ApplicationTypes.Configuration
  ( DynamicConfigDatum(DynamicConfigDatum)
  )
import ScriptArguments.Types
  ( ConfigPolicyParams(ConfigPolicyParams)
  , TallyPolicyParams
  , ValidatorParams(ValidatorParams)
  )

-- | Create config result
newtype CreateConfigResult = CreateConfigResult
  { txHash :: TransactionHash
  , indexSymbol :: ScriptHash
  , indexTokenName :: AssetName
  , configSymbol :: ScriptHash
  , configTokenName :: AssetName
  , tallySymbol :: ScriptHash
  }

-- | Contract for creating dynamic config datum and locking
-- | it at UTXO at config validator marked by config NFT
createConfig ::
  CreateConfigParams ->
  Contract CreateConfigResult
createConfig params = do
  logInfo' "Entering createConfig transaction"

  userUtxos <- getAllWalletUtxos

  configSpend <- liftContractM "No UTXOs found"
    $ head
    $ Map.toUnfoldable userUtxos

  dynamicConfigInfo <-
    buildDynamicConfig
      params
      configSpend

  let
    lookups :: Lookups.ScriptLookups
    lookups = dynamicConfigInfo.lookups

    constraints :: Constraints.TxConstraints
    constraints = dynamicConfigInfo.constraints

    indexSymbol :: ScriptHash
    indexSymbol = params # unwrap # _.indexSymbol

    indexTokenName :: AssetName
    indexTokenName = params # unwrap # _.indexTokenName

    configSymbol :: ScriptHash
    configSymbol = dynamicConfigInfo.symbol

    configTokenName :: AssetName
    configTokenName = params # unwrap # _.configTokenName

    tallySymbol :: ScriptHash
    tallySymbol = dynamicConfigInfo.tallySymbol

  txHash <- submitTxFromConstraints lookups constraints

  pure $ CreateConfigResult
    { txHash
    , indexSymbol
    , indexTokenName
    , configSymbol
    , configTokenName
    , tallySymbol
    }

type ConfigInfo =
  { symbol :: ScriptHash
  , tallySymbol :: ScriptHash
  , lookups :: Lookups.ScriptLookups
  , constraints :: Constraints.TxConstraints
  }

-- Build the lookups and constraints for the transaction
buildDynamicConfig ::
  CreateConfigParams ->
  (TransactionInput /\ TransactionOutput) ->
  Contract ConfigInfo
buildDynamicConfig params' (txInput /\ txInputWithScript) =
  do
    logInfo' "Entering buildDynamicConfig transaction"

    let
      params = params' # unwrap

      configPolicyParams :: ConfigPolicyParams
      configPolicyParams = ConfigPolicyParams
        { cpInitialUtxo: txInput, cpTokenName: params.configTokenName }

    appliedConfigPolicy :: PlutusScript <- unappliedConfigPolicy
      configPolicyParams

    let
      configSymbol :: ScriptHash
      configSymbol = PlutusScript.hash appliedConfigPolicy

      configValidatorParams :: ValidatorParams
      configValidatorParams =
        ValidatorParams
          { vpConfigSymbol: configSymbol
          , vpConfigTokenName: params.configTokenName
          }

      tallyConfig :: TallyPolicyParams
      tallyConfig = mkTallyConfig configSymbol
        params.indexSymbol
        params.configTokenName
        params.indexTokenName

    appliedConfigValidator :: PlutusScript <- unappliedConfigValidator
      configValidatorParams

    -- Make the scripts for the dynamic config datum
    appliedTreasuryValidator :: PlutusScript <- unappliedTreasuryValidator
      configValidatorParams
    appliedTallyValidator :: PlutusScript <- unappliedTallyValidator
      configValidatorParams
    appliedVoteValidator :: PlutusScript <- unappliedVoteValidator
      configValidatorParams
    appliedVotePolicy :: PlutusScript <- unappliedVotePolicy
      configValidatorParams
    voteNftPolicy' :: PlutusScript <- voteNftPolicy
    appliedTallyPolicy :: PlutusScript <- unappliedTallyPolicy tallyConfig

    let
      tallyScriptHash :: ScriptHash
      tallyScriptHash = PlutusScript.hash appliedTallyValidator

      treasuryScriptHash :: ScriptHash
      treasuryScriptHash = PlutusScript.hash appliedTreasuryValidator

      voteScriptHash :: ScriptHash
      voteScriptHash = PlutusScript.hash appliedVoteValidator

      configScriptHash :: ScriptHash
      configScriptHash = PlutusScript.hash appliedConfigValidator

      voteNftSymbol :: ScriptHash
      voteNftSymbol = PlutusScript.hash voteNftPolicy'

      voteSymbol :: ScriptHash
      voteSymbol = PlutusScript.hash appliedVotePolicy

      tallyNftSymbol :: ScriptHash
      tallyNftSymbol = PlutusScript.hash appliedTallyPolicy

      dynamicConfig :: DynamicConfigDatum
      dynamicConfig = DynamicConfigDatum
        { -- Scripts
          tallyValidator: tallyScriptHash
        , configurationValidator: configScriptHash
        , voteValidator: voteScriptHash
        , treasuryValidator: treasuryScriptHash

        -- Percentages and thresholds
        , upgradeMajorityPercent: params.upgradeMajorityPercent
        , upgradeRelativeMajorityPercent:
            params.upgradeRelativeMajorityPercent
        , generalMajorityPercent: params.generalMajorityPercent
        , generalRelativeMajorityPercent:
            params.generalRelativeMajorityPercent
        , tripMajorityPercent: params.tripMajorityPercent
        , tripRelativeMajorityPercent: params.tripRelativeMajorityPercent
        , totalVotes: params.totalVotes
        , maxGeneralDisbursement: params.maxGeneralDisbursement
        , maxTripDisbursement: params.maxTripDisbursement
        , agentDisbursementPercent: params.agentDisbursementPercent
        , proposalTallyEndOffset: params.proposalTallyEndOffset
        , fungibleVotePercent: params.fungibleVotePercent

        -- Symbols and token names
        , tallyNft: tallyNftSymbol
        , voteCurrencySymbol: voteSymbol
        , voteTokenName: params.voteTokenName
        , voteNft: voteNftSymbol
        , voteFungibleCurrencySymbol: params.voteFungibleCurrencySymbol
        , voteFungibleTokenName: params.voteFungibleTokenName
        }

    let
      -- We need to pay the config to the config validator so we require its hash
      configValidatorHash :: ScriptHash
      configValidatorHash = PlutusScript.hash appliedConfigValidator

      -- This NFT is used to mark the UTXO at the
      -- config validator containing the config
      nftConfig :: Value
      nftConfig = Value.singleton configSymbol params.configTokenName BigNum.one

      configDatum :: PlutusData
      configDatum = toData dynamicConfig

      lookups' :: Lookups.ScriptLookups
      lookups' = mconcat
        [ Lookups.plutusMintingPolicy appliedConfigPolicy
        , Lookups.unspentOutputs $ Map.singleton txInput txInputWithScript
        ]

      constraints' :: Constraints.TxConstraints
      constraints' = mconcat
        [ Constraints.mustMintValue $ Mint.fromMultiAsset $ Value.getMultiAsset
            nftConfig
        , Constraints.mustSpendPubKeyOutput txInput
        , Constraints.mustPayToScript
            configValidatorHash
            configDatum
            Constraints.DatumInline
            nftConfig
        -- ^ We pay the newly created config to a UTXO at the config
        -- validator, marked by the 'nftConfig'
        ]

    pure
      { symbol: configSymbol
      , tallySymbol: tallyNftSymbol
      , lookups: lookups'
      , constraints: constraints'
      }

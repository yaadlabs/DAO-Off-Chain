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
  , unwrap
  , (#)
  , ($)
  , (/\)
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts
  ( MintingPolicy
  , ScriptHash
  , Validator
  , ValidatorHash
  , validatorHash
  )
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
import Dao.Component.Config.Params (CreateConfigParams)
import Dao.Component.Tally.Params (mkTallyConfig)
import Dao.Scripts.Policy.Config (unappliedConfigPolicyDebug)
import Dao.Scripts.Policy.Tally (unappliedTallyPolicyDebug)
import Dao.Scripts.Policy.Vote (unappliedVotePolicyDebug)
import Dao.Scripts.Policy.VoteNft (voteNftPolicy)
import Dao.Scripts.Validator.Config
  ( unappliedConfigValidatorDebug
  )
import Dao.Scripts.Validator.Tally (unappliedTallyValidatorDebug)
import Dao.Scripts.Validator.Treasury (unappliedTreasuryValidator)
import Dao.Scripts.Validator.Vote (unappliedVoteValidatorDebug)
import Dao.Utils.Contract (ContractResult(ContractResult))
import Dao.Utils.Query (getAllWalletUtxos)
import Data.Array (head)
import Data.Map as Map
import LambdaBuffers.ApplicationTypes.Configuration
  ( DynamicConfigDatum(DynamicConfigDatum)
  )
import ScriptArguments.Types
  ( ConfigurationValidatorConfig(ConfigurationValidatorConfig)
  , NftConfig(NftConfig)
  , TallyNftConfig
  )

-- | Contract for creating dynamic config datum and locking
-- | it at UTXO at config validator marked by config NFT
createConfig ::
  CreateConfigParams ->
  Contract ContractResult
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

    symbol :: CurrencySymbol
    symbol = dynamicConfigInfo.symbol

    tokenName :: TokenName
    tokenName = params # unwrap # _.configTokenName

  txHash <- submitTxFromConstraints lookups constraints

  pure $ ContractResult
    { txHash, symbol, tokenName }

type ConfigInfo =
  { symbol :: CurrencySymbol
  , lookups :: Lookups.ScriptLookups
  , constraints :: Constraints.TxConstraints
  }

buildDynamicConfig ::
  CreateConfigParams ->
  (TransactionInput /\ TransactionOutputWithRefScript) ->
  Contract ConfigInfo
buildDynamicConfig params' (txInput /\ txInputWithScript) =
  do
    logInfo' "Entering buildDynamicConfig transaction"

    let
      params = params' # unwrap

      configPolicyParams :: NftConfig
      configPolicyParams = NftConfig
        { ncInitialUtxo: txInput, ncTokenName: params.configTokenName }

    appliedConfigPolicy :: MintingPolicy <- unappliedConfigPolicyDebug
      configPolicyParams

    let
      configSymbol :: CurrencySymbol
      configSymbol = scriptCurrencySymbol appliedConfigPolicy

      configValidatorParams :: ConfigurationValidatorConfig
      configValidatorParams =
        ConfigurationValidatorConfig
          { cvcConfigNftCurrencySymbol: configSymbol
          , cvcConfigNftTokenName: params.configTokenName
          }

      tallyConfig :: TallyNftConfig
      tallyConfig = mkTallyConfig configSymbol
        params.indexSymbol
        params.configTokenName
        params.indexTokenName

    appliedConfigValidator :: Validator <- unappliedConfigValidatorDebug
      configValidatorParams

    -- Make the scripts for the dynamic config datum
    appliedTreasuryValidator :: Validator <- unappliedTreasuryValidator
      configValidatorParams
    appliedTallyValidator :: Validator <- unappliedTallyValidatorDebug
      configValidatorParams
    appliedVoteValidator :: Validator <- unappliedVoteValidatorDebug
      configValidatorParams
    appliedVotePolicy :: MintingPolicy <- unappliedVotePolicyDebug
      configValidatorParams
    voteNftPolicy' :: MintingPolicy <- voteNftPolicy
    appliedTallyPolicy :: MintingPolicy <- unappliedTallyPolicyDebug tallyConfig

    let
      tallyScriptHash :: ScriptHash
      tallyScriptHash = unwrap $ validatorHash appliedTallyValidator

      treasuryScriptHash :: ScriptHash
      treasuryScriptHash = unwrap $ validatorHash appliedTreasuryValidator

      voteScriptHash :: ScriptHash
      voteScriptHash = unwrap $ validatorHash appliedVoteValidator

      configScriptHash :: ScriptHash
      configScriptHash = unwrap $ validatorHash appliedConfigValidator

      voteNftSymbol :: CurrencySymbol
      voteNftSymbol = scriptCurrencySymbol voteNftPolicy'

      voteSymbol :: CurrencySymbol
      voteSymbol = scriptCurrencySymbol appliedVotePolicy

      tallyNftSymbol :: CurrencySymbol
      tallyNftSymbol = scriptCurrencySymbol appliedTallyPolicy

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
      configValidatorHash :: ValidatorHash
      configValidatorHash = validatorHash appliedConfigValidator

      nftConfig :: Value
      nftConfig = Value.singleton configSymbol params.configTokenName one

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

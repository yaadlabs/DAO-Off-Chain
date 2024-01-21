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
import Dao.Component.Config.Params (ConfigParams)
import Dao.Component.Tally.Params (mkTallyConfig)
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
import Scripts.ConfigPolicy (unappliedConfigPolicyDebug)
import Scripts.ConfigValidator
  ( unappliedConfigValidatorDebug
  )
import Scripts.TallyPolicy (unappliedTallyPolicyDebug)
import Scripts.TallyValidator (unappliedTallyValidatorDebug)
import Scripts.TreasuryValidator (unappliedTreasuryValidator)
import Scripts.VoteNft (voteNftPolicy)
import Scripts.VoteValidator (unappliedVoteValidatorDebug)

-- | Contract for creating dynamic config datum and locking
-- it at UTXO at config validator marked by config NFT
createConfig ::
  ConfigParams ->
  Contract (TransactionHash /\ CurrencySymbol /\ TokenName)
createConfig configParams = do
  logInfo' "Entering createConfig transaction"

  userUtxos <- getAllWalletUtxos

  configSpend <- liftContractM "No UTXOs found"
    $ head
    $ Map.toUnfoldable userUtxos

  dynamicConfigInfo <-
    buildDynamicConfig
      configParams
      configSpend

  let
    lookups :: Lookups.ScriptLookups
    lookups = dynamicConfigInfo.lookups

    constraints :: Constraints.TxConstraints
    constraints = dynamicConfigInfo.constraints

  txHash <- submitTxFromConstraints lookups constraints

  pure (txHash /\ dynamicConfigInfo.symbol /\ configParams.configTokenName)

type ConfigInfo =
  { symbol :: CurrencySymbol
  , lookups :: Lookups.ScriptLookups
  , constraints :: Constraints.TxConstraints
  }

buildDynamicConfig ::
  ConfigParams ->
  (TransactionInput /\ TransactionOutputWithRefScript) ->
  Contract ConfigInfo
buildDynamicConfig configParams (txInput /\ txInputWithScript) =
  do
    logInfo' "Entering buildDynamicConfig transaction"

    let
      configPolicyParams :: NftConfig
      configPolicyParams = NftConfig
        { ncInitialUtxo: txInput, ncTokenName: configParams.configTokenName }

    appliedConfigPolicy :: MintingPolicy <- unappliedConfigPolicyDebug
      configPolicyParams

    let
      configSymbol :: CurrencySymbol
      configSymbol = scriptCurrencySymbol appliedConfigPolicy

      configValidatorParams :: ConfigurationValidatorConfig
      configValidatorParams =
        ConfigurationValidatorConfig
          { cvcConfigNftCurrencySymbol: configSymbol
          , cvcConfigNftTokenName: configParams.configTokenName
          }

      tallyConfig :: TallyNftConfig
      tallyConfig = mkTallyConfig configSymbol
        configParams.indexSymbol
        configParams.configTokenName
        configParams.indexTokenName

    appliedConfigValidator :: Validator <- unappliedConfigValidatorDebug
      configValidatorParams

    -- Make the scripts for the dynamic config datum
    appliedTreasuryValidator :: Validator <- unappliedTreasuryValidator
      configValidatorParams
    appliedTallyValidator :: Validator <- unappliedTallyValidatorDebug
      configValidatorParams
    appliedVoteValidator :: Validator <- unappliedVoteValidatorDebug
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
        , upgradeMajorityPercent: configParams.upgradeMajorityPercent
        , upgradeRelativeMajorityPercent:
            configParams.upgradeRelativeMajorityPercent
        , generalMajorityPercent: configParams.generalMajorityPercent
        , generalRelativeMajorityPercent:
            configParams.generalRelativeMajorityPercent
        , tripMajorityPercent: configParams.tripMajorityPercent
        , tripRelativeMajorityPercent: configParams.tripRelativeMajorityPercent
        , totalVotes: configParams.totalVotes
        , maxGeneralDisbursement: configParams.maxGeneralDisbursement
        , maxTripDisbursement: configParams.maxTripDisbursement
        , agentDisbursementPercent: configParams.agentDisbursementPercent
        , proposalTallyEndOffset: configParams.proposalTallyEndOffset
        , fungibleVotePercent: configParams.fungibleVotePercent

        -- Symbols and token names
        , tallyNft: tallyNftSymbol
        , voteCurrencySymbol: configParams.voteCurrencySymbol
        , voteTokenName: configParams.voteTokenName
        , voteNft: voteNftSymbol
        , voteFungibleCurrencySymbol: configParams.voteFungibleCurrencySymbol
        , voteFungibleTokenName: configParams.voteFungibleTokenName
        }

    let
      configValidatorHash :: ValidatorHash
      configValidatorHash = validatorHash appliedConfigValidator

      nftConfig :: Value
      nftConfig = Value.singleton configSymbol configParams.configTokenName one

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

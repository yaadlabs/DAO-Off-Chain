{-|
Module: Dao.Workflow.TreasuryTrip
Description: Contract for disbursing treasury funds based on a trip proposal
-}
module Dao.Workflow.TreasuryTrip (treasuryTrip) where

import Contract.Address (Address, scriptHashAddress)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedM)
import Contract.PlutusData
  ( Datum(Datum)
  , Redeemer(Redeemer)
  , toData
  , unitDatum
  , unitRedeemer
  )
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
import Contract.Scripts (Validator, ValidatorHash, validatorHash)
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
  )
import Contract.Value (singleton) as Value
import Dao.Utils.Query (findUtxoByValue)
import Data.Map as Map
import Data.Maybe (Maybe(Nothing))
import JS.BigInt as BigInt
import LambdaBuffers.ApplicationTypes.Arguments
  ( ConfigurationValidatorConfig(ConfigurationValidatorConfig)
  )
import LambdaBuffers.ApplicationTypes.Configuration (DynamicConfigDatum)
import LambdaBuffers.ApplicationTypes.Proposal (ProposalType(ProposalType'Trip))
-- import ScriptArguments.Types
--   ( ConfigurationValidatorConfig(ConfigurationValidatorConfig)
--   )
import Scripts.ConfigValidator (unappliedConfigValidator)
import Scripts.TallyValidator (unappliedTallyValidator)
import Scripts.TreasuryValidator (unappliedTreasuryValidator)

type TallyInfo = { tallySymbol :: CurrencySymbol, tallyTokenName :: TokenName }
type TreasuryInfo =
  { treasurySymbol :: CurrencySymbol
  , treasuryTokenName :: TokenName
  , travelAgentAddress :: Address
  , travellerAddress :: Address
  }

treasuryTrip ::
  ConfigurationValidatorConfig ->
  TallyInfo ->
  TreasuryInfo ->
  DynamicConfigDatum ->
  Contract TransactionHash
treasuryTrip validatorConfig tallyInfo treasuryInfo newDynamicConfigDatum = do
  logInfo' "Entering treasuryTrip transaction"

  appliedTreasuryValidator :: Validator <- unappliedTreasuryValidator
    validatorConfig
  appliedTallyValidator :: Validator <- unappliedTallyValidator validatorConfig
  appliedConfigValidator :: Validator <- unappliedConfigValidator
    validatorConfig

  let
    configValidatorAddress = scriptHashAddress
      (validatorHash appliedConfigValidator)
      Nothing
  configValidatorUtxoMap <- utxosAt configValidatorAddress

  let
    tallyValidatorAddress = scriptHashAddress
      (validatorHash appliedTallyValidator)
      Nothing
  tallyValidatorUtxoMap <- utxosAt tallyValidatorAddress

  let
    treasuryValidatorAddress = scriptHashAddress
      (validatorHash appliedTreasuryValidator)
      Nothing
  treasuryValidatorUtxoMap <- utxosAt treasuryValidatorAddress

  (configUtxoTxInput /\ configUtxoTxOutRefScript) <-
    liftedM "Could not find config UTXO" $ getConfigUtxo validatorConfig
      configValidatorUtxoMap

  (treasuryUtxoTxInput /\ treasuryUtxoTxOutRefScript) <-
    liftedM "Could not find treasury UTXO" $ getTreasuryUtxo treasuryInfo
      treasuryValidatorUtxoMap

  (tallyUtxoTxInput /\ _) <-
    liftedM "Could not find tally UTXO" $ getTallyUtxo tallyInfo
      tallyValidatorUtxoMap

  let
    -- TODO: Just a placeholder
    totalCost :: BigInt.BigInt
    totalCost = BigInt.fromInt 5

    newConfigDatum :: Datum
    newConfigDatum = Datum $ toData newDynamicConfigDatum

    treasuryNft :: Value
    treasuryNft = Value.singleton (treasuryInfo.treasurySymbol)
      (treasuryInfo.treasuryTokenName)
      one

    treasuryValidatorHash :: ValidatorHash
    treasuryValidatorHash = validatorHash appliedTreasuryValidator

    treasuryRedeemer :: Redeemer
    treasuryRedeemer = Redeemer $ toData $ ProposalType'Trip
      (treasuryInfo.travelAgentAddress)
      (treasuryInfo.travellerAddress)
      totalCost

    lookups :: Lookups.ScriptLookups
    lookups =
      mconcat
        [ Lookups.unspentOutputs $ Map.singleton treasuryUtxoTxInput
            treasuryUtxoTxOutRefScript
        ]

    constraints :: Constraints.TxConstraints
    constraints =
      mconcat
        [ Constraints.mustPayToScript
            treasuryValidatorHash
            unitDatum
            Constraints.DatumInline
            treasuryNft
        , Constraints.mustSpendScriptOutput treasuryUtxoTxInput treasuryRedeemer
        , Constraints.mustReferenceOutput tallyUtxoTxInput
        , Constraints.mustReferenceOutput configUtxoTxInput
        ]

  txHash <- submitTxFromConstraints lookups constraints

  pure txHash
  where
  getConfigUtxo ::
    ConfigurationValidatorConfig ->
    UtxoMap ->
    Contract (Maybe (TransactionInput /\ TransactionOutputWithRefScript))
  getConfigUtxo
    ( ConfigurationValidatorConfig
        { cvcConfigNftCurrencySymbol, cvcConfigNftTokenName }
    ) =
    findUtxoByValue
      (Value.singleton cvcConfigNftCurrencySymbol cvcConfigNftTokenName one)

  getTallyUtxo ::
    TallyInfo ->
    UtxoMap ->
    Contract (Maybe (TransactionInput /\ TransactionOutputWithRefScript))
  getTallyUtxo ({ tallySymbol, tallyTokenName }) =
    findUtxoByValue
      (Value.singleton tallySymbol tallyTokenName one)

  getTreasuryUtxo ::
    TreasuryInfo ->
    UtxoMap ->
    Contract (Maybe (TransactionInput /\ TransactionOutputWithRefScript))
  getTreasuryUtxo ({ treasurySymbol, treasuryTokenName }) =
    findUtxoByValue
      (Value.singleton treasurySymbol treasuryTokenName one)

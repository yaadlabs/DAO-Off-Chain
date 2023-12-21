{-|
Module: Dao.Workflow.CountVote
Description: Contract for counting a vote on a proposal
-}
module Dao.Workflow.CountVote (countVote) where

import Contract.Address (scriptHashAddress)
import Contract.Chain (currentTime)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.PlutusData
  ( Datum(Datum)
  , Redeemer(Redeemer)
  , toData
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
  , (*)
  , (+)
  , (/\)
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy, Validator, ValidatorHash, validatorHash)
import Contract.Time
  ( Interval(FiniteInterval)
  , POSIXTime(POSIXTime)
  , POSIXTimeRange
  )
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
import Dao.Utils.Datum (getInlineDatumFromTxOutWithRefScript)
import Dao.Utils.Query (findUtxoByValue)
import Dao.Utils.Time (mkOnchainTimeRange, mkTimeRangeWithinSummary)
import Data.Map as Map
import Data.Maybe (Maybe(Nothing))
import JS.BigInt (fromInt)
import LambdaBuffers.ApplicationTypes.Vote
  ( VoteActionRedeemer(VoteActionRedeemer'Count)
  , VoteDatum
  )
import ScriptArguments.Types
  ( ConfigurationValidatorConfig(ConfigurationValidatorConfig)
  )
import Scripts.ConfigValidator (unappliedConfigValidator)
import Scripts.TallyValidator (unappliedTallyValidator)
import Scripts.VotePolicy (unappliedVotePolicy)
import Scripts.VoteValidator (unappliedVoteValidator)

type VoteInfo = { voteSymbol :: CurrencySymbol, voteTokenName :: TokenName }
type TallyInfo = { tallySymbol :: CurrencySymbol, tallyTokenName :: TokenName }

countVote ::
  ConfigurationValidatorConfig ->
  VoteInfo ->
  TallyInfo ->
  Contract TransactionHash
countVote validatorConfig voteInfo tallyInfo = do
  logInfo' "Entering countVote transaction"

  appliedConfigValidator :: Validator <- unappliedConfigValidator
    validatorConfig
  appliedTallyValidator :: Validator <- unappliedTallyValidator validatorConfig
  appliedVoteValidator :: Validator <- unappliedConfigValidator
    validatorConfig

  let
    configValidatorAddress = scriptHashAddress
      (validatorHash appliedConfigValidator)
      Nothing

    tallyValidatorAddress = scriptHashAddress
      (validatorHash appliedTallyValidator)
      Nothing

    voteValidatorAddress = scriptHashAddress
      (validatorHash appliedVoteValidator)
      Nothing

  configValidatorUtxoMap <- utxosAt configValidatorAddress
  tallyValidatorUtxoMap <- utxosAt tallyValidatorAddress
  voteValidatorUtxoMap <- utxosAt voteValidatorAddress

  (configUtxoTxInput /\ _) <-
    liftedM "Could not find config UTXO" $ getConfigUtxo validatorConfig
      configValidatorUtxoMap

  (tallyUtxoTxInput /\ tallyUtxoTxOutRefScript) <-
    liftedM "Could not find tally UTXO" $ getTallyUtxo tallyInfo
      tallyValidatorUtxoMap

  (voteUtxoTxInput /\ voteUtxoTxOutRefScript) <-
    liftedM "Could not find vote UTXO" $ getVoteUtxo voteInfo
      voteValidatorUtxoMap

  tallyStateDatum :: Datum <-
    liftContractM "No Inline tally datum at OutputDatum" $
      getInlineDatumFromTxOutWithRefScript tallyUtxoTxOutRefScript

  voteDatum :: Datum <-
    liftContractM "No Inline vote datum at OutputDatum" $
      getInlineDatumFromTxOutWithRefScript voteUtxoTxOutRefScript

  let
    voteNft :: Value
    voteNft = Value.singleton voteInfo.voteSymbol voteInfo.voteTokenName one

    tallyNft :: Value
    tallyNft = Value.singleton tallyInfo.tallySymbol tallyInfo.tallyTokenName
      one

    voteValidatorHash :: ValidatorHash
    voteValidatorHash = validatorHash appliedVoteValidator

    tallyValidatorHash :: ValidatorHash
    tallyValidatorHash = validatorHash appliedTallyValidator

    countRedeemer :: Redeemer
    countRedeemer = Redeemer $ toData VoteActionRedeemer'Count

    lookups :: Lookups.ScriptLookups
    lookups = mconcat
      [ Lookups.unspentOutputs $ Map.singleton tallyUtxoTxInput
          tallyUtxoTxOutRefScript
      , Lookups.unspentOutputs $ Map.singleton voteUtxoTxInput
          voteUtxoTxOutRefScript
      ]

    constraints :: Constraints.TxConstraints
    constraints =
      mconcat
        [ Constraints.mustPayToScript
            voteValidatorHash
            voteDatum
            Constraints.DatumInline
            voteNft
        , Constraints.mustPayToScript
            tallyValidatorHash
            tallyStateDatum
            Constraints.DatumInline
            tallyNft
        , Constraints.mustReferenceOutput configUtxoTxInput
        , Constraints.mustSpendScriptOutput tallyUtxoTxInput unitRedeemer
        , Constraints.mustSpendScriptOutput voteUtxoTxInput countRedeemer
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

  getVoteUtxo ::
    VoteInfo ->
    UtxoMap ->
    Contract (Maybe (TransactionInput /\ TransactionOutputWithRefScript))
  getVoteUtxo ({ voteSymbol, voteTokenName }) =
    findUtxoByValue
      (Value.singleton voteSymbol voteTokenName one)

  mkVoteValidityRange :: Contract POSIXTimeRange
  mkVoteValidityRange = do
    currentTime' <- currentTime
    let
      -- Five minutes
      timePeriod = POSIXTime (fromInt $ 1000 * 60 * 5)
      endTime = currentTime' + timePeriod

      timeRange :: POSIXTimeRange
      timeRange = FiniteInterval currentTime' endTime

    mkTimeRangeWithinSummary timeRange

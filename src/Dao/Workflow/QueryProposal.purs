{-|
Module: Dao.Workflow.QueryProposal
Description: Contracts for querying proposals
-}
module Dao.Workflow.QueryProposal
  ( getAllProposals
  , getAllGeneralProposals
  , getAllTripProposals
  , getAllUpgradeProposals
  , getAllActiveProposals
  , getAllExpiredProposals
  , getAllSuccessfulProposals
  ) where

import Contract.Prelude

import Contract.Address (Address, scriptHashAddress)
import Contract.Log (logInfo')
import Contract.Monad (Contract)
import Contract.Scripts (MintingPolicy, Validator, validatorHash)
import Contract.Time (POSIXTime)
import Contract.Transaction
  ( TransactionInput
  , TransactionOutputWithRefScript
  )
import Contract.Utxos (utxosAt)
import Contract.Value (CurrencySymbol, scriptCurrencySymbol)
import Dao.Component.Config.Params (mkValidatorConfig)
import Dao.Component.Config.Query (ConfigInfo, referenceConfigUtxo)
import Dao.Component.Proposal.Params (QueryProposalParams)
import Dao.Component.Tally.Params (mkTallyConfig)
import Dao.Scripts.Policy.Tally (unappliedTallyPolicyDebug)
import Dao.Scripts.Validator.Config (unappliedConfigValidatorDebug)
import Dao.Scripts.Validator.Tally (unappliedTallyValidatorDebug)
import Dao.Utils.Datum (extractOutputDatum)
import Dao.Utils.Query (hasTokenWithSymbol)
import Dao.Utils.Time (getCurrentTime)
import Data.Array (filter, mapMaybe)
import Data.Map as Map
import JS.BigInt (BigInt, fromInt)
import LambdaBuffers.ApplicationTypes.Configuration (DynamicConfigDatum)
import LambdaBuffers.ApplicationTypes.Proposal
  ( ProposalType
      ( ProposalType'General
      , ProposalType'Trip
      , ProposalType'Upgrade
      )
  )
import LambdaBuffers.ApplicationTypes.Tally (TallyStateDatum)
import Type.Proxy (Proxy(Proxy))

-- | Retrieve all proposals
getAllProposals ::
  QueryProposalParams ->
  Contract (Array TallyStateDatum)
getAllProposals params' = do
  logInfo' "Entering getAllProposals contract"

  let params = params' # unwrap

  -- Make the scripts
  let
    validatorConfig = mkValidatorConfig params.configSymbol
      params.configTokenName
  appliedTallyValidator :: Validator <- unappliedTallyValidatorDebug
    validatorConfig

  -- Make the tally policy script
  let
    tallyConfig = mkTallyConfig params.configSymbol
      params.indexSymbol
      params.configTokenName
      params.indexTokenName
  appliedTallyPolicy :: MintingPolicy <- unappliedTallyPolicyDebug tallyConfig

  let
    tallySymbol :: CurrencySymbol
    tallySymbol = scriptCurrencySymbol appliedTallyPolicy

    tallyValidatorAddress :: Address
    tallyValidatorAddress = scriptHashAddress
      (validatorHash appliedTallyValidator)
      Nothing

  tallyValidatorUtxos <- utxosAt tallyValidatorAddress

  let
    proposalUtxos :: Array TallyStateDatum
    proposalUtxos = getProposalInfo $ filter (hasTokenWithSymbol tallySymbol)
      (Map.toUnfoldable tallyValidatorUtxos)

  pure proposalUtxos
  where
  getProposalInfo ::
    Array (TransactionInput /\ TransactionOutputWithRefScript) ->
    Array TallyStateDatum
  getProposalInfo = mapMaybe op
    where
    op (_ /\ txOut) = extractOutputDatum (Proxy :: Proxy TallyStateDatum) txOut

-- | Retrieve all proposals of type 'General'
getAllGeneralProposals ::
  QueryProposalParams ->
  Contract (Array TallyStateDatum)
getAllGeneralProposals params = do
  logInfo' "Entering getAllGeneralProposals contract"
  getProposalsByType params General

-- | Retrieve all proposals of type 'General'
getAllTripProposals ::
  QueryProposalParams ->
  Contract (Array TallyStateDatum)
getAllTripProposals params = do
  logInfo' "Entering getAllTripProposals contract"
  getProposalsByType params Trip

-- | Retrieve all proposals of type 'Upgrade'
getAllUpgradeProposals ::
  QueryProposalParams ->
  Contract (Array TallyStateDatum)
getAllUpgradeProposals params = do
  logInfo' "Entering getAllUpgradeProposals contract"
  getProposalsByType params Upgrade

data ProposalType' = General | Upgrade | Trip

derive instance Eq ProposalType'

getProposalsByType ::
  QueryProposalParams ->
  ProposalType' ->
  Contract (Array TallyStateDatum)
getProposalsByType params proposalType = do
  allProposals <- getAllProposals params
  pure $ filter (\p -> isType proposalType (p # unwrap # _.proposal))
    allProposals
  where
  isType :: ProposalType' -> ProposalType -> Boolean
  isType General (ProposalType'General _ _) = true
  isType Trip (ProposalType'Trip _ _ _) = true
  isType Upgrade (ProposalType'Upgrade _) = true
  isType _ _ = false

-- | Retrieve all active proposals
getAllActiveProposals ::
  QueryProposalParams ->
  Contract (Array TallyStateDatum)
getAllActiveProposals params = do
  logInfo' "Entering getAllActiveProposals contract"
  getProposalsByTime params isActiveProposal

-- | Retrieve all expired proposals
-- | Voting not possible after the
-- | 'proposalEndTime' has passed
getAllExpiredProposals ::
  QueryProposalParams ->
  Contract (Array TallyStateDatum)
getAllExpiredProposals params = do
  logInfo' "Entering getAllExpiredProposals contract"
  getProposalsByTime params (not <<< isActiveProposal)

getProposalsByTime ::
  QueryProposalParams ->
  (POSIXTime -> TallyStateDatum -> Boolean) ->
  Contract (Array TallyStateDatum)
getProposalsByTime params condition = do
  allProposals <- getAllProposals params
  currentTime <- getCurrentTime
  pure $ filter (condition currentTime) allProposals

isActiveProposal :: POSIXTime -> TallyStateDatum -> Boolean
isActiveProposal currentTime tallyDatum =
  (tallyDatum # unwrap # _.proposalEndTime) > currentTime

-- | Get all successful proposals
getAllSuccessfulProposals ::
  QueryProposalParams ->
  Contract (Array TallyStateDatum)
getAllSuccessfulProposals params = do
  logInfo' "Entering getAllSuccessfulProposals contract"

  -- Get the config UTXO
  let
    params' = params # unwrap
    validatorConfig = mkValidatorConfig params'.configSymbol
      params'.configTokenName
  appliedConfigValidator :: Validator <- unappliedConfigValidatorDebug
    validatorConfig
  configInfo :: ConfigInfo <- referenceConfigUtxo params'.configSymbol
    appliedConfigValidator

  -- The main config referenced at the config UTXO
  let
    configDatum :: DynamicConfigDatum
    configDatum = configInfo.datum

  allExpiredProposals <- getAllExpiredProposals params
  pure $ filter (isSuccessfulProposal configDatum) allExpiredProposals

isSuccessfulProposal :: DynamicConfigDatum -> TallyStateDatum -> Boolean
isSuccessfulProposal configDatum tallyDatum =
  case tallyDatum # unwrap # _.proposal of
    ProposalType'General _ _ -> isSuccessfulGeneralProposal configDatum
      tallyDatum
    ProposalType'Trip _ _ _ -> isSuccessfulTripProposal configDatum tallyDatum
    ProposalType'Upgrade _ -> isSuccessfulUpgradeProposal configDatum
      tallyDatum

isSuccessfulGeneralProposal ::
  DynamicConfigDatum ->
  TallyStateDatum ->
  Boolean
isSuccessfulGeneralProposal configDatum tallyDatum =
  let
    (relativeMajority /\ majorityPercent) = getMajorities configDatum tallyDatum

    configGeneralRelativeMajorityPercent :: BigInt
    configGeneralRelativeMajorityPercent = configDatum # unwrap #
      _.generalRelativeMajorityPercent

    configGeneralMajorityPercent :: BigInt
    configGeneralMajorityPercent = configDatum # unwrap #
      _.generalMajorityPercent
  in
    -- Check for sufficient votes by ensuring the config thresholds are exceeded
    relativeMajority >= configGeneralRelativeMajorityPercent && majorityPercent
      >= configGeneralMajorityPercent

isSuccessfulTripProposal ::
  DynamicConfigDatum ->
  TallyStateDatum ->
  Boolean
isSuccessfulTripProposal configDatum tallyDatum =
  let
    (relativeMajority /\ majorityPercent) = getMajorities configDatum tallyDatum

    configTripRelativeMajorityPercent :: BigInt
    configTripRelativeMajorityPercent = configDatum # unwrap #
      _.tripRelativeMajorityPercent

    configTripMajorityPercent :: BigInt
    configTripMajorityPercent = configDatum # unwrap # _.tripMajorityPercent
  in
    -- Check for sufficient votes
    relativeMajority >= configTripRelativeMajorityPercent && majorityPercent >=
      configTripMajorityPercent

isSuccessfulUpgradeProposal ::
  DynamicConfigDatum ->
  TallyStateDatum ->
  Boolean
isSuccessfulUpgradeProposal configDatum tallyDatum =
  let
    (relativeMajority /\ majorityPercent) = getMajorities configDatum tallyDatum

    configUpgradeRelativeMajorityPercent :: BigInt
    configUpgradeRelativeMajorityPercent = configDatum # unwrap #
      _.upgradeRelativeMajorityPercent

    configUpgradeMajorityPercent :: BigInt
    configUpgradeMajorityPercent = configDatum # unwrap #
      _.upgradeMajorityPercent
  in
    -- Check for sufficient votes
    relativeMajority >= configUpgradeRelativeMajorityPercent && majorityPercent
      >= configUpgradeMajorityPercent

getMajorities :: DynamicConfigDatum -> TallyStateDatum -> (BigInt /\ BigInt)
getMajorities configDatum tallyDatum =
  let
    votesFor :: BigInt
    votesFor = tallyDatum # unwrap # _.for

    votesAgainst :: BigInt
    votesAgainst = tallyDatum # unwrap # _.against

    totalVotes :: BigInt
    totalVotes = votesFor + votesAgainst

    configTotalVotes :: BigInt
    configTotalVotes = configDatum # unwrap # _.totalVotes

    relativeMajority :: BigInt
    relativeMajority = (totalVotes * (fromInt 1000)) / configTotalVotes

    majorityPercent :: BigInt
    majorityPercent = (votesFor * (fromInt 1000)) / totalVotes
  in
    (relativeMajority /\ majorityPercent)

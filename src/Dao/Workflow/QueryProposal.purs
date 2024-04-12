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
  , getProposalByTokenName
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
import Contract.Value (CurrencySymbol, TokenName, scriptCurrencySymbol)
import Dao.Component.Config.Params (mkValidatorConfig)
import Dao.Component.Config.Query (ConfigInfo, referenceConfigUtxo)
import Dao.Component.Proposal.Params (QueryProposalParams)
import Dao.Component.Proposal.Query
  ( QueryResult(QueryResult)
  , getTokenNameAndDatumFromOutput
  )
import Dao.Component.Tally.Params (mkTallyConfig)
import Dao.Scripts.Policy.Tally (unappliedTallyPolicy)
import Dao.Scripts.Validator.Config (unappliedConfigValidator)
import Dao.Scripts.Validator.Tally (unappliedTallyValidator)
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

-- | Retrieve an individual proposal by its token name
getProposalByTokenName ::
  QueryProposalParams ->
  TokenName ->
  Contract (Maybe QueryResult)
getProposalByTokenName params proposalName = do
  logInfo' "Entering getProposalByTokenName contract"
  allProposals <- getAllProposals params
  case check proposalName allProposals of
    [ queryResult ] -> pure $ Just queryResult
    _ -> pure Nothing
  where
  check :: TokenName -> Array QueryResult -> Array QueryResult
  check proposalName = mapMaybe op
    where
    op queryResult =
      if (queryResult # unwrap # _.proposalTokenName) == proposalName then Just
        queryResult
      else Nothing

-- | Retrieve all proposals
getAllProposals ::
  QueryProposalParams ->
  Contract (Array QueryResult)
getAllProposals params' = do
  logInfo' "Entering getAllProposals contract"

  let params = params' # unwrap

  -- Make the scripts
  let
    validatorConfig = mkValidatorConfig params.configSymbol
      params.configTokenName
  appliedTallyValidator :: Validator <- unappliedTallyValidator
    validatorConfig

  -- Make the tally policy script
  let
    tallyConfig = mkTallyConfig params.configSymbol
      params.indexSymbol
      params.configTokenName
      params.indexTokenName
  appliedTallyPolicy :: MintingPolicy <- unappliedTallyPolicy tallyConfig

  let
    tallySymbol :: CurrencySymbol
    tallySymbol = scriptCurrencySymbol appliedTallyPolicy

    tallyValidatorAddress :: Address
    tallyValidatorAddress = scriptHashAddress
      (validatorHash appliedTallyValidator)
      Nothing

  tallyValidatorUtxos <- utxosAt tallyValidatorAddress

  let
    proposalUtxos :: Array QueryResult
    proposalUtxos = getProposalInfo tallySymbol $ filter
      (hasTokenWithSymbol tallySymbol)
      (Map.toUnfoldable tallyValidatorUtxos)

  pure proposalUtxos
  where
  getProposalInfo ::
    CurrencySymbol ->
    Array (TransactionInput /\ TransactionOutputWithRefScript) ->
    Array QueryResult
  getProposalInfo symbol = mapMaybe op
    where
    op = getTokenNameAndDatumFromOutput symbol

-- | Retrieve all proposals of type 'General'
getAllGeneralProposals ::
  QueryProposalParams ->
  Contract (Array QueryResult)
getAllGeneralProposals params = do
  logInfo' "Entering getAllGeneralProposals contract"
  getProposalsByType params General

-- | Retrieve all proposals of type 'General'
getAllTripProposals ::
  QueryProposalParams ->
  Contract (Array QueryResult)
getAllTripProposals params = do
  logInfo' "Entering getAllTripProposals contract"
  getProposalsByType params Trip

-- | Retrieve all proposals of type 'Upgrade'
getAllUpgradeProposals ::
  QueryProposalParams ->
  Contract (Array QueryResult)
getAllUpgradeProposals params = do
  logInfo' "Entering getAllUpgradeProposals contract"
  getProposalsByType params Upgrade

data ProposalType' = General | Upgrade | Trip

derive instance Eq ProposalType'

getProposalsByType ::
  QueryProposalParams ->
  ProposalType' ->
  Contract (Array QueryResult)
getProposalsByType params proposalType = do
  allProposals <- getAllProposals params
  pure $ filter (check proposalType) allProposals
  where
  isType :: ProposalType' -> ProposalType -> Boolean
  isType General (ProposalType'General _ _) = true
  isType Trip (ProposalType'Trip _ _ _) = true
  isType Upgrade (ProposalType'Upgrade _) = true
  isType _ _ = false

  check :: ProposalType' -> QueryResult -> Boolean
  check propType' queryResult = isType propType' $ queryResult # unwrap
    # _.tallyDatum
    # unwrap
    # _.proposal

-- | Retrieve all active proposals
getAllActiveProposals ::
  QueryProposalParams ->
  Contract (Array QueryResult)
getAllActiveProposals params = do
  logInfo' "Entering getAllActiveProposals contract"
  getProposalsByTime params isActiveProposal

-- | Retrieve all expired proposals
-- | Voting not possible after the
-- | 'proposalEndTime' has passed
getAllExpiredProposals ::
  QueryProposalParams ->
  Contract (Array QueryResult)
getAllExpiredProposals params = do
  logInfo' "Entering getAllExpiredProposals contract"
  getProposalsByTime params (not <<< isActiveProposal)

getProposalsByTime ::
  QueryProposalParams ->
  (POSIXTime -> QueryResult -> Boolean) ->
  Contract (Array QueryResult)
getProposalsByTime params condition = do
  allProposals <- getAllProposals params
  currentTime <- getCurrentTime
  pure $ filter (condition currentTime) allProposals

isActiveProposal :: POSIXTime -> QueryResult -> Boolean
isActiveProposal currentTime queryResult =
  (queryResult # unwrap # _.tallyDatum # unwrap # _.proposalEndTime) >
    currentTime

-- | Get all successful proposals
getAllSuccessfulProposals ::
  QueryProposalParams ->
  Contract (Array QueryResult)
getAllSuccessfulProposals params = do
  logInfo' "Entering getAllSuccessfulProposals contract"

  -- Get the config UTXO
  let
    params' = params # unwrap
    validatorConfig = mkValidatorConfig params'.configSymbol
      params'.configTokenName
  appliedConfigValidator :: Validator <- unappliedConfigValidator
    validatorConfig
  configInfo :: ConfigInfo <- referenceConfigUtxo params'.configSymbol
    appliedConfigValidator

  -- The main config referenced at the config UTXO
  let
    configDatum :: DynamicConfigDatum
    configDatum = configInfo.datum

  allExpiredProposals <- getAllExpiredProposals params
  pure $ filter (isSuccessfulProposal configDatum) allExpiredProposals

isSuccessfulProposal :: DynamicConfigDatum -> QueryResult -> Boolean
isSuccessfulProposal configDatum queryResult =
  case tallyDatum # unwrap # _.proposal of
    ProposalType'General _ _ -> isSuccessfulGeneralProposal configDatum
      tallyDatum
    ProposalType'Trip _ _ _ -> isSuccessfulTripProposal configDatum tallyDatum
    ProposalType'Upgrade _ -> isSuccessfulUpgradeProposal configDatum
      tallyDatum
  where
  tallyDatum = queryResult # unwrap # _.tallyDatum

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

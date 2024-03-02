module Dao.Workflow.QueryProposal
  ( getAllProposals
  , getAllGeneralProposals
  , getAllTripProposals
  , getAllUpgradeProposals
  , getAllActiveProposals
  , getAllExpiredProposals
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
import Dao.Component.Proposal.Params (QueryProposalParams)
import Dao.Component.Tally.Params (mkTallyConfig)
import Dao.Scripts.Policy.Tally (unappliedTallyPolicyDebug)
import Dao.Scripts.Validator.Tally (unappliedTallyValidatorDebug)
import Dao.Utils.Datum (extractOutputDatum)
import Dao.Utils.Query (hasTokenWithSymbol)
import Dao.Utils.Time (getCurrentTime)
import Data.Array (filter, mapMaybe)
import Data.Map as Map
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

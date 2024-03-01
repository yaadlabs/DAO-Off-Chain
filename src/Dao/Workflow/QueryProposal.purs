module Dao.Workflow.QueryProposal (getAllProposals) where

import Contract.Prelude

import Contract.Address (Address, scriptHashAddress)
import Contract.Log (logInfo')
import Contract.Monad (Contract)
import Contract.Scripts (MintingPolicy, Validator, validatorHash)
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
import Data.Array (filter, mapMaybe)
import Data.Map as Map
import LambdaBuffers.ApplicationTypes.Tally (TallyStateDatum)
import Type.Proxy (Proxy(Proxy))

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

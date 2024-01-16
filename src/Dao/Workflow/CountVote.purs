{-|
Module: Dao.Workflow.CountVote
Description: Contract for counting a vote on a proposal
-}
module Dao.Workflow.CountVote (countVote) where

import Contract.Address (scriptHashAddress)
import Contract.Log (logInfo')
import Contract.Monad (Contract)
import Contract.PlutusData
  ( Datum(Datum)
  , toData
  )
import Contract.Prelude
  ( type (/\)
  , bind
  , discard
  , foldMap
  , foldr
  , mconcat
  , otherwise
  , pure
  , unwrap
  , (#)
  , ($)
  , (*)
  , (+)
  , (/\)
  , (==)
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy, Validator, ValidatorHash, validatorHash)
import Contract.Time (POSIXTime(POSIXTime))
import Contract.Transaction
  ( TransactionHash
  , TransactionInput
  , TransactionOutputWithRefScript
  , submitTxFromConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Dao.Component.Config.Params (mkValidatorConfig)
import Dao.Component.Config.Query (ConfigInfo, referenceConfigUtxo)
import Dao.Component.Tally.Query (TallyInfo, spendTallyUtxo)
import Dao.Component.Vote.Params (CountVoteParams)
import Dao.Component.Vote.Query (mkAllVoteConstraintsAndLookups)
import Dao.Scripts.Policy.VotePolicy (unappliedVotePolicy)
import Dao.Scripts.Validator.ConfigValidator (unappliedConfigValidator)
import Dao.Scripts.Validator.TallyValidator (unappliedTallyValidator)
import Dao.Scripts.Validator.VoteValidator (unappliedVoteValidator)
import Dao.Utils.Time (mkOnchainTimeRange, mkValidityRange, oneMinute)
import Data.Map (Map)
import Data.Maybe (Maybe(Nothing))
import JS.BigInt (BigInt, fromInt)
import LambdaBuffers.ApplicationTypes.Tally (TallyStateDatum(TallyStateDatum))
import LambdaBuffers.ApplicationTypes.Vote (VoteDirection(VoteDirection'For))

-- | Contract for counting the votes
-- TODO: Include fungible token calculation
countVote ::
  CountVoteParams ->
  Contract TransactionHash
countVote params' = do
  logInfo' "Entering countVote transaction"

  let params = params' # unwrap

  -- Make the scripts
  let
    validatorConfig = mkValidatorConfig params.configSymbol
      params.configTokenName

  appliedConfigValidator :: Validator <- unappliedConfigValidator
    validatorConfig
  appliedTallyValidator :: Validator <- unappliedTallyValidator validatorConfig
  appliedVoteValidator :: Validator <- unappliedVoteValidator
    validatorConfig
  appliedVotePolicy :: MintingPolicy <- unappliedVotePolicy validatorConfig

  -- Query the UTXOs
  configInfo :: ConfigInfo <- referenceConfigUtxo params.configSymbol
    appliedConfigValidator
  tallyInfo :: TallyInfo <- spendTallyUtxo params.tallySymbol
    appliedTallyValidator

  let
    scriptAddr = scriptHashAddress (validatorHash appliedVoteValidator) Nothing
  voteUtxos :: Map TransactionInput TransactionOutputWithRefScript <- utxosAt
    scriptAddr

  -- Collect the constraints and lookups for each vote UTXO
  -- And whether the vote was for or against
  voteDirectionsConstraintsAndLookups ::
    Array (VoteDirection /\ Lookups.ScriptLookups /\ Constraints.TxConstraints) <-
    mkAllVoteConstraintsAndLookups
      params.voteNftSymbol
      params.voteSymbol
      params.voteNftTokenName
      params.voteTokenName
      appliedVotePolicy
      voteUtxos

  -- Make on-chain time range
  timeRange <- mkValidityRange (POSIXTime $ fromInt $ 5 * oneMinute)
  onchainTimeRange <- mkOnchainTimeRange timeRange

  let
    -- Get the total votes for and against
    (votesFor /\ votesAgainst) = tallyVotes voteDirectionsConstraintsAndLookups

    -- Update the tally datum with the new vote count
    tallyDatumWithUpdateVoteCount :: TallyStateDatum
    tallyDatumWithUpdateVoteCount =
      let
        oldDatum = tallyInfo.datum # unwrap
      in
        TallyStateDatum
          { proposal: oldDatum.proposal
          , proposalEndTime: oldDatum.proposalEndTime
          , for: oldDatum.for + votesFor
          , against: oldDatum.against + votesAgainst
          }

    tallyValidatorHash :: ValidatorHash
    tallyValidatorHash = validatorHash appliedTallyValidator

    -- Collect the vote lookups
    voteLookups :: Lookups.ScriptLookups
    voteLookups = foldMap (\(_ /\ lookups' /\ _) -> lookups')
      voteDirectionsConstraintsAndLookups

    -- Collect the vote constraints
    voteConstraints :: Constraints.TxConstraints
    voteConstraints = foldMap (\(_ /\ _ /\ constraints') -> constraints')
      voteDirectionsConstraintsAndLookups

    lookups :: Lookups.ScriptLookups
    lookups = mconcat
      [ voteLookups
      , tallyInfo.lookups
      , configInfo.lookups
      ]

    constraints :: Constraints.TxConstraints
    constraints =
      mconcat
        [ Constraints.mustPayToScript
            tallyValidatorHash
            (Datum $ toData $ tallyDatumWithUpdateVoteCount)
            Constraints.DatumInline
            tallyInfo.value
        , voteConstraints
        , configInfo.constraints
        , tallyInfo.constraints
        , Constraints.mustValidateIn onchainTimeRange
        ]

  txHash <- submitTxFromConstraints lookups constraints

  pure txHash
  where
  tallyVotes ::
    Array (VoteDirection /\ Lookups.ScriptLookups /\ Constraints.TxConstraints) ->
    (BigInt /\ BigInt)
  tallyVotes = foldr op (fromInt 0 /\ fromInt 0)
    where
    op (voteDirection /\ _ /\ _) (for /\ against)
      | voteDirection == VoteDirection'For = ((for + fromInt 1) /\ against)
      | otherwise = (for /\ (against + fromInt 1))

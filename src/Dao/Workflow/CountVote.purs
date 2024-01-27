{-|
Module: Dao.Workflow.CountVote
Description: Contract for counting a vote on a proposal
-}
module Dao.Workflow.CountVote (countVote) where

import Contract.Address (scriptHashAddress)
import Contract.Chain (waitNSlots)
import Contract.Log (logInfo')
import Contract.Monad (Contract)
import Contract.Numeric.Natural as Natural
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
  , void
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
import Contract.Value (CurrencySymbol, TokenName)
import Dao.Component.Config.Params (mkValidatorConfig)
import Dao.Component.Config.Query (ConfigInfo, referenceConfigUtxo)
import Dao.Component.Tally.Query (TallyInfo, spendTallyUtxo)
import Dao.Component.Vote.Params (CountVoteParams)
import Dao.Component.Vote.Query (mkAllVoteConstraintsAndLookups)
import Dao.Scripts.Policy.Vote (unappliedVotePolicyDebug)
import Dao.Scripts.Validator.Config (unappliedConfigValidatorDebug)
import Dao.Scripts.Validator.Tally (unappliedTallyValidatorDebug)
import Dao.Scripts.Validator.Vote (unappliedVoteValidatorDebug)
import Dao.Utils.Time (mkOnchainTimeRange, mkValidityRange, oneMinute)
import Data.Map (Map)
import Data.Maybe (Maybe(Nothing))
import JS.BigInt (BigInt, fromInt)
import LambdaBuffers.ApplicationTypes.Configuration (DynamicConfigDatum)
import LambdaBuffers.ApplicationTypes.Tally (TallyStateDatum(TallyStateDatum))
import LambdaBuffers.ApplicationTypes.Vote (VoteDirection(VoteDirection'For))

-- | Contract for counting the votes
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
  appliedConfigValidator :: Validator <- unappliedConfigValidatorDebug
    validatorConfig
  appliedTallyValidator :: Validator <- unappliedTallyValidatorDebug
    validatorConfig
  appliedVoteValidator :: Validator <- unappliedVoteValidatorDebug
    validatorConfig
  appliedVotePolicy :: MintingPolicy <- unappliedVotePolicyDebug validatorConfig

  -- Query the UTXOs
  configInfo :: ConfigInfo <- referenceConfigUtxo params.configSymbol
    appliedConfigValidator
  tallyInfo :: TallyInfo <- spendTallyUtxo params.tallySymbol
    appliedTallyValidator

  let
    scriptAddr = scriptHashAddress (validatorHash appliedVoteValidator) Nothing
  voteUtxos :: Map TransactionInput TransactionOutputWithRefScript <- utxosAt
    scriptAddr

  let
    configDatum :: DynamicConfigDatum
    configDatum = configInfo.datum

    fungibleSymbol :: CurrencySymbol
    fungibleSymbol = configDatum # unwrap # _.voteFungibleCurrencySymbol

    voteNftSymbol :: CurrencySymbol
    voteNftSymbol = configDatum # unwrap # _.voteNft

    voteSymbol :: CurrencySymbol
    voteSymbol = configDatum # unwrap # _.voteCurrencySymbol

    voteTokenName :: TokenName
    voteTokenName = configDatum # unwrap # _.voteTokenName

    fungiblePercent :: BigInt
    fungiblePercent = configDatum # unwrap # _.fungibleVotePercent

  -- Collect the constraints and lookups for each vote UTXO
  -- And whether the vote was for or against
  voteDirectionsConstraintsAndLookups ::
    Array
      ( (VoteDirection /\ BigInt) /\ Lookups.ScriptLookups /\
          Constraints.TxConstraints
      ) <-
    mkAllVoteConstraintsAndLookups
      voteNftSymbol
      voteSymbol
      fungibleSymbol
      voteTokenName
      fungiblePercent
      appliedVotePolicy
      voteUtxos

  -- Make on-chain time range
  timeRange <- mkValidityRange (POSIXTime $ fromInt $ 5 * oneMinute)
  onchainTimeRange <- mkOnchainTimeRange timeRange

  -- Hack to work around Ogmios submitted too early error (in Plutip test)
  void $ waitNSlots (Natural.fromInt' 10)

  let
    -- Get the total votes for and against
    (votesFor /\ votesAgainst) = tallyVotes voteDirectionsConstraintsAndLookups

    -- Update the tally datum with the new vote count
    tallyDatumWithUpdatedVoteCount :: TallyStateDatum
    tallyDatumWithUpdatedVoteCount =
      let
        oldDatum = tallyInfo.datum # unwrap
      in
        TallyStateDatum
          { proposal: oldDatum.proposal
          , proposalEndTime: oldDatum.proposalEndTime
          , for: oldDatum.for + votesFor
          , against: oldDatum.against + votesAgainst
          }

  let
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
      , Lookups.validator appliedVoteValidator
      ]

    constraints :: Constraints.TxConstraints
    constraints =
      mconcat
        [ Constraints.mustPayToScript
            tallyValidatorHash
            (Datum $ toData $ tallyDatumWithUpdatedVoteCount)
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
    Array
      ( (VoteDirection /\ BigInt) /\ Lookups.ScriptLookups /\
          Constraints.TxConstraints
      ) ->
    (BigInt /\ BigInt)
  tallyVotes = foldr op (fromInt 0 /\ fromInt 0)
    where
    op ((voteDirection /\ voteAmount) /\ _ /\ _) (for /\ against)
      | voteDirection == VoteDirection'For = ((for + voteAmount) /\ against)
      | otherwise = (for /\ (against + voteAmount))

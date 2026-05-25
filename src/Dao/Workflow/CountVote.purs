{-|
Module: Dao.Workflow.CountVote
Description: Contract for counting a vote on a proposal
-}
module Dao.Workflow.CountVote (countVote) where

import Prelude

import Cardano.ToData (toData)
import Cardano.Types
  ( Address
  , AssetName
  , Credential(ScriptHashCredential)
  , PlutusScript
  , ScriptHash
  , TransactionHash
  , TransactionInput
  , TransactionOutput
  )
import Cardano.Types.Address (mkPaymentAddress)
import Cardano.Types.BigNum (fromInt) as BigNum
import Cardano.Types.PlutusScript (hash) as PlutusScript
import Contract.Address (getNetworkId)
import Contract.Chain (waitNSlots)
import Contract.Log (logInfo')
import Contract.Monad (Contract)
import Contract.Prelude
  ( type (/\)
  , foldMap
  , foldr
  , mconcat
  , unwrap
  , (/\)
  )
import Contract.ScriptLookups as Lookups
import Contract.Time (POSIXTime(POSIXTime))
import Contract.Transaction (submitTxFromConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Dao.Component.Config.Params (mkValidatorConfig)
import Dao.Component.Config.Query (ConfigInfo, referenceConfigUtxo)
import Dao.Component.Tally.Query (TallyInfo, spendTallyUtxo)
import Dao.Component.Vote.Params (CountVoteParams)
import Dao.Component.Vote.Query (mkAllVoteConstraintsAndLookups)
import Dao.Scripts.Policy (unappliedVotePolicy)
import Dao.Scripts.Validator
  ( unappliedConfigValidator
  , unappliedTallyValidator
  , unappliedVoteValidator
  )
import Dao.Utils.Time (mkOnchainTimeRange, mkValidityRange, oneMinute)
import Dao.Workflow.ReferenceScripts (retrieveReferenceScript)
import Data.Map (Map)
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (wrap)
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

  logInfo' $ "countVoteParams:\n" <> show params

  -- Make the scripts
  let
    validatorConfig = mkValidatorConfig params.configSymbol
      params.configTokenName
  appliedConfigValidator :: PlutusScript <- unappliedConfigValidator
    validatorConfig
  appliedTallyValidator :: PlutusScript <- unappliedTallyValidator
    validatorConfig
  appliedVoteValidator :: PlutusScript <- unappliedVoteValidator
    validatorConfig
  votePolicyScript :: PlutusScript <- unappliedVotePolicy validatorConfig

  tallyValidatorRef <- retrieveReferenceScript appliedTallyValidator
  voteValidatorRef <- retrieveReferenceScript appliedVoteValidator
  votePolicyRef <- retrieveReferenceScript votePolicyScript

  let votePolicyHash = PlutusScript.hash votePolicyScript

  -- Query the UTXOs
  configInfo :: ConfigInfo <- referenceConfigUtxo params.configSymbol
    appliedConfigValidator
  tallyInfo :: TallyInfo <- spendTallyUtxo params.tallySymbol
    params.proposalTokenName
    appliedTallyValidator
    tallyValidatorRef

  network <- getNetworkId

  let
    -- The main config referenced at the config UTXO
    configDatum :: DynamicConfigDatum
    configDatum = configInfo.datum

    voteValidatorHash :: ScriptHash
    voteValidatorHash = configDatum # unwrap # _.voteValidator

    -- We need the address of the vote validator in order to retrieve the vote UTXOs
    voteValidatorAddress :: Address
    voteValidatorAddress =
      mkPaymentAddress
        network
        (wrap $ ScriptHashCredential voteValidatorHash)
        Nothing

  -- Get the UTXOs at the vote validator
  voteUtxos :: Map TransactionInput TransactionOutput <- utxosAt
    voteValidatorAddress
  logInfo' $ "voteUtxos: " <> show voteUtxos

  -- Extract the config values
  let
    -- The 'voteSymbol' is the symbol of the 'votePolicy'
    -- used when a user votes on a proposal
    voteSymbol :: ScriptHash
    voteSymbol = configDatum # unwrap # _.voteCurrencySymbol

    -- The token name for the token created with the 'voteSymbol'
    voteTokenName :: AssetName
    voteTokenName = configDatum # unwrap # _.voteTokenName

    -- The symbol of the vote 'pass'
    -- A user requires this token in order to vote on a proposal
    voteNftSymbol :: ScriptHash
    voteNftSymbol = configDatum # unwrap # _.voteNft

    -- The symbol of the vote 'multiplier' token
    fungibleSymbol :: ScriptHash
    fungibleSymbol = configDatum # unwrap # _.voteFungibleCurrencySymbol

    -- This percentage is used when calculating the value of the user's
    -- fungible tokens in terms of how much it will add to the weight of their vote
    -- The calculation at the script is:
    -- (fungibleTokens * fungibleVotePercent) `divide` 1000
    fungiblePercent :: BigInt
    fungiblePercent = configDatum # unwrap # _.fungibleVotePercent

  -- Collect the constraints and lookups for each vote UTXO
  -- Includes whether the vote was for or against the proposal,
  -- and the weight each vote has (fungible tokens can increase vote weight)
  voteDirectionsConstraintsAndLookups ::
    Array
      ( (VoteDirection /\ BigInt) /\ Lookups.ScriptLookups /\
          Constraints.TxConstraints
      ) <-
    mkAllVoteConstraintsAndLookups
      voteNftSymbol
      voteSymbol
      fungibleSymbol
      params.proposalTokenName
      voteTokenName
      fungiblePercent
      votePolicyHash
      voteValidatorRef
      votePolicyRef
      voteUtxos

  logInfo' $ "voteConstraints: " <> show voteDirectionsConstraintsAndLookups

  -- Make on-chain time range
  timeRange <- mkValidityRange (POSIXTime $ fromInt $ 5 * oneMinute)
  onchainTimeRange <- mkOnchainTimeRange timeRange

  -- Hack to work around Ogmios submitted too early error (in Plutip test)
  void $ waitNSlots $ BigNum.fromInt 10

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
    -- We need to pay the updated datum with its corresponding token
    -- to the tally validator, hence we need its hash
    tallyValidatorHash :: ScriptHash
    tallyValidatorHash = configDatum # unwrap # _.tallyValidator

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
            (toData tallyDatumWithUpdatedVoteCount)
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
  -- Calculate the total number of votes for and against
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

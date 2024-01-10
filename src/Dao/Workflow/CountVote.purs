{-|
Module: Dao.Workflow.CountVote
Description: Contract for counting a vote on a proposal
-}
module Dao.Workflow.CountVote (countVote) where

import Contract.Log (logInfo')
import Contract.Monad (Contract)
import Contract.PlutusData
  ( Datum(Datum)
  , toData
  )
import Contract.Prelude
  ( bind
  , discard
  , mconcat
  , pure
  , ($)
  , (*)
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts (Validator, ValidatorHash, validatorHash)
import Contract.Time (POSIXTime(POSIXTime))
import Contract.Transaction
  ( TransactionHash
  , submitTxFromConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Value (CurrencySymbol)
import Dao.Component.Config.Query (ConfigInfo, referenceConfigUtxo)
import Dao.Component.Tally.Query (TallyInfo, spendTallyUtxo)
import Dao.Component.Vote.Query (VoteInfo, spendVoteUtxo)
import Dao.Utils.Time (mkOnchainTimeRange, mkValidityRange, oneMinute)
import JS.BigInt (fromInt)
import LambdaBuffers.ApplicationTypes.Arguments (ConfigurationValidatorConfig)
import LambdaBuffers.ApplicationTypes.Vote
  ( VoteActionRedeemer(VoteActionRedeemer'Count)
  )
import Scripts.ConfigValidator (unappliedConfigValidator)
import Scripts.TallyValidator (unappliedTallyValidator)
import Scripts.VoteValidator (unappliedVoteValidator)

-- | Contract for vote count
countVote ::
  ConfigurationValidatorConfig ->
  CurrencySymbol ->
  CurrencySymbol ->
  CurrencySymbol ->
  Contract TransactionHash
countVote validatorConfig configSymbol voteSymbol tallySymbol = do
  logInfo' "Entering countVote transaction"

  -- Make the scripts
  appliedConfigValidator :: Validator <- unappliedConfigValidator
    validatorConfig
  appliedTallyValidator :: Validator <- unappliedTallyValidator validatorConfig
  appliedVoteValidator :: Validator <- unappliedVoteValidator
    validatorConfig

  -- Query the UTXOs
  configInfo :: ConfigInfo <- referenceConfigUtxo configSymbol
    appliedConfigValidator
  tallyInfo :: TallyInfo <- spendTallyUtxo tallySymbol appliedTallyValidator
  voteInfo :: VoteInfo <- spendVoteUtxo VoteActionRedeemer'Count voteSymbol
    appliedVoteValidator

  -- Make on-chain time range
  timeRange <- mkValidityRange (POSIXTime $ fromInt $ 5 * oneMinute)
  onchainTimeRange <- mkOnchainTimeRange timeRange

  let
    voteValidatorHash :: ValidatorHash
    voteValidatorHash = validatorHash appliedVoteValidator

    tallyValidatorHash :: ValidatorHash
    tallyValidatorHash = validatorHash appliedTallyValidator

    lookups :: Lookups.ScriptLookups
    lookups = mconcat
      [ configInfo.lookups
      , voteInfo.lookups
      , tallyInfo.lookups
      ]

    constraints :: Constraints.TxConstraints
    constraints =
      mconcat
        [ Constraints.mustPayToScript
            voteValidatorHash
            (Datum $ toData $ voteInfo.datum)
            Constraints.DatumInline
            voteInfo.value
        , Constraints.mustPayToScript
            tallyValidatorHash
            (Datum $ toData $ tallyInfo.datum)
            Constraints.DatumInline
            tallyInfo.value
        , configInfo.constraints
        , voteInfo.constraints
        , tallyInfo.constraints
        , Constraints.mustValidateIn onchainTimeRange
        ]

  txHash <- submitTxFromConstraints lookups constraints

  pure txHash

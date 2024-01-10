{-|
Module: Dao.Workflow.CancelVote
Description: Contract for cancelling a vote on a proposal
-}
module Dao.Workflow.CancelVote (cancelVote) where

import Contract.Address (PaymentPubKeyHash)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData
  ( Redeemer(Redeemer)
  , toData
  )
import Contract.Prelude
  ( bind
  , discard
  , mconcat
  , negate
  , one
  , pure
  , (#)
  , ($)
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy, Validator)
import Contract.Transaction
  ( TransactionHash
  , submitTxFromConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , Value
  )
import Contract.Value (singleton) as Value
import Dao.Component.Config.Query (ConfigInfo, referenceConfigUtxo)
import Dao.Component.Vote.Query (VoteInfo, spendVoteUtxo)
import Dao.Utils.Address (addressToPaymentPubKeyHash)
import Data.Newtype (unwrap)
import LambdaBuffers.ApplicationTypes.Arguments (ConfigurationValidatorConfig)
import LambdaBuffers.ApplicationTypes.Vote
  ( VoteActionRedeemer(VoteActionRedeemer'Cancel)
  , VoteMinterActionRedeemer(VoteMinterActionRedeemer'Burn)
  )
import Scripts.ConfigValidator (unappliedConfigValidator)
import Scripts.VotePolicy (unappliedVotePolicy)
import Scripts.VoteValidator (unappliedVoteValidator)

-- | Contract for cancelling a vote
cancelVote ::
  ConfigurationValidatorConfig ->
  CurrencySymbol ->
  CurrencySymbol ->
  TokenName ->
  Contract TransactionHash
cancelVote validatorConfig configSymbol voteSymbol voteTokenName = do
  logInfo' "Entering cancelVote transaction"

  -- Make the scripts
  appliedVotePolicy :: MintingPolicy <- unappliedVotePolicy validatorConfig
  appliedVoteValidator :: Validator <- unappliedVoteValidator validatorConfig
  appliedConfigValidator :: Validator <- unappliedConfigValidator
    validatorConfig

  -- Query the UTXOs
  configInfo :: ConfigInfo <- referenceConfigUtxo configSymbol
    appliedConfigValidator
  voteInfo :: VoteInfo <- spendVoteUtxo VoteActionRedeemer'Cancel voteSymbol
    appliedVoteValidator

  -- Extract the vote owner from the vote datum
  voteOwnerKey :: PaymentPubKeyHash <-
    liftContractM "Could not convert address to key"
      $ addressToPaymentPubKeyHash
      $ voteInfo.datum
      # unwrap
      # _.voteOwner

  let
    burnVoteNft :: Value
    burnVoteNft = Value.singleton voteSymbol voteTokenName (negate one)

    burnVoteRedeemer :: Redeemer
    burnVoteRedeemer = Redeemer $ toData VoteMinterActionRedeemer'Burn

    lookups :: Lookups.ScriptLookups
    lookups =
      mconcat
        [ Lookups.mintingPolicy appliedVotePolicy
        , voteInfo.lookups
        , configInfo.lookups
        ]

    constraints :: Constraints.TxConstraints
    constraints =
      mconcat
        [ Constraints.mustMintValueWithRedeemer burnVoteRedeemer burnVoteNft
        , Constraints.mustBeSignedBy voteOwnerKey
        , configInfo.constraints
        , voteInfo.constraints
        ]

  txHash <- submitTxFromConstraints lookups constraints

  pure txHash

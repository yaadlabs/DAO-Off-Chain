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
  , mempty
  , negate
  , one
  , otherwise
  , pure
  , zero
  , (#)
  , ($)
  , (<>)
  , (==)
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
  , Value
  , scriptCurrencySymbol
  )
import Contract.Value (singleton) as Value
import Dao.Component.Config.Params (mkValidatorConfig)
import Dao.Component.Config.Query (ConfigInfo, referenceConfigUtxo)
import Dao.Component.Vote.Params (CancelVoteParams)
import Dao.Component.Vote.Query (VoteInfo, spendVoteUtxo)
import Dao.Scripts.Policy.Vote (unappliedVotePolicyDebug)
import Dao.Scripts.Validator.Config (unappliedConfigValidatorDebug)
import Dao.Scripts.Validator.Vote (unappliedVoteValidatorDebug)
import Dao.Utils.Address (addressToPaymentPubKeyHash)
import Dao.Utils.Value (countOfTokenInValue)
import Data.Newtype (unwrap)
import JS.BigInt (BigInt)
import LambdaBuffers.ApplicationTypes.Vote
  ( VoteActionRedeemer(VoteActionRedeemer'Cancel)
  , VoteMinterActionRedeemer(VoteMinterActionRedeemer'Burn)
  )

-- | Contract for cancelling a vote
cancelVote ::
  CancelVoteParams ->
  Contract TransactionHash
cancelVote params' = do
  logInfo' "Entering cancelVote transaction"

  let params = params' # unwrap

  -- Make the scripts
  let
    validatorConfig = mkValidatorConfig params.configSymbol
      params.configTokenName
  appliedVotePolicy :: MintingPolicy <- unappliedVotePolicyDebug validatorConfig
  appliedVoteValidator :: Validator <- unappliedVoteValidatorDebug
    validatorConfig
  appliedConfigValidator :: Validator <- unappliedConfigValidatorDebug
    validatorConfig

  let
    voteSymbol :: CurrencySymbol
    voteSymbol = scriptCurrencySymbol appliedVotePolicy

  -- Query the UTXOs
  configInfo :: ConfigInfo <- referenceConfigUtxo params.configSymbol
    appliedConfigValidator
  voteInfo :: VoteInfo <- spendVoteUtxo VoteActionRedeemer'Cancel
    voteSymbol
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
    burnVoteNft = Value.singleton voteSymbol params.voteTokenName
      (negate one)

    voteNftPass :: Value
    voteNftPass = Value.singleton params.voteNftSymbol params.voteNftTokenName
      one

    burnVoteRedeemer :: Redeemer
    burnVoteRedeemer = Redeemer $ toData VoteMinterActionRedeemer'Burn

    fungibleAmount :: BigInt
    fungibleAmount = countOfTokenInValue params.fungibleSymbol voteInfo.value

    fungibleToken :: Value
    fungibleToken
      | fungibleAmount == zero = mempty
      | otherwise = Value.singleton params.fungibleSymbol
          params.fungibleTokenName
          fungibleAmount

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
        , Constraints.mustPayToPubKey voteOwnerKey
            (voteNftPass <> fungibleToken)
        -- ^ Pay the vote 'pass' back to the owner, and the fungibleTokens if any
        , configInfo.constraints
        , voteInfo.constraints
        ]

  txHash <- submitTxFromConstraints lookups constraints

  pure txHash

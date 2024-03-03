{-|
Module: Dao.Workflow.CancelVote
Description: Contract for cancelling a vote on a proposal
-}
module Dao.Workflow.CancelVote (cancelVote) where

import Contract.Address (PaymentPubKeyHash)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedM)
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
  , TokenName
  , Value
  )
import Contract.Value (singleton) as Value
import Contract.Wallet (ownPaymentPubKeyHash)
import Dao.Component.Config.Params (mkValidatorConfig)
import Dao.Component.Config.Query (ConfigInfo, referenceConfigUtxo)
import Dao.Component.Vote.Params (CancelVoteParams)
import Dao.Component.Vote.Query (VoteInfo, cancelVoteUtxo)
import Dao.Scripts.Policy.Vote (unappliedVotePolicyDebug)
import Dao.Scripts.Validator.Config (unappliedConfigValidatorDebug)
import Dao.Scripts.Validator.Vote (unappliedVoteValidatorDebug)
import Dao.Utils.Address (addressToPaymentPubKeyHash)
import Dao.Utils.Value (countOfTokenInValue, mkTokenName)
import Data.Newtype (unwrap)
import JS.BigInt (BigInt)
import LambdaBuffers.ApplicationTypes.Configuration (DynamicConfigDatum)
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

  -- Query the UTXOs
  configInfo :: ConfigInfo <- referenceConfigUtxo params.configSymbol
    appliedConfigValidator

  let
    -- The main config referenced at the config UTXO
    configDatum :: DynamicConfigDatum
    configDatum = configInfo.datum

    -- The 'voteSymbol' is the symbol of the 'votePolicy'
    -- used when a user votes on a proposal
    voteSymbol :: CurrencySymbol
    voteSymbol = configDatum # unwrap # _.voteCurrencySymbol

    -- The token name for the token created with the 'voteSymbol'
    voteTokenName :: TokenName
    voteTokenName = configDatum # unwrap # _.voteTokenName

    -- We need to burn the vote token created and sent to
    -- the user when they voted on the proposal
    burnVoteNft :: Value
    burnVoteNft = Value.singleton voteSymbol voteTokenName
      (negate one)

    -- The 'votePolicy' minting policy takes two possible redeemers, Mint or Burn
    -- In this case we wish to burn the token we minted when voting on the proposal
    burnVoteRedeemer :: Redeemer
    burnVoteRedeemer = Redeemer $ toData VoteMinterActionRedeemer'Burn

  -- We get the user's own PKH in order to spend the correct
  -- vote UTXO, the one belonging to this user
  userPkh :: PaymentPubKeyHash <- liftedM "Could not get own PKH"
    ownPaymentPubKeyHash

  -- Spend the specific vote UTXO owned by this user
  -- Also ensure that the vote was for the specific proposal passed as an argument
  voteInfo :: VoteInfo <- cancelVoteUtxo VoteActionRedeemer'Cancel voteSymbol
    userPkh
    params.proposalTokenName
    appliedVoteValidator

  -- Extract the vote owner from the vote datum
  -- Should be equivalent to result of 'ownPaymentPubKeyHash',
  -- otherwise 'cancelVoteUtxo' would have have failed
  voteOwnerKey :: PaymentPubKeyHash <-
    liftContractM "Could not convert address to key"
      $ addressToPaymentPubKeyHash
      $ voteInfo.datum
      # unwrap
      # _.voteOwner

  -- TODO: Add this field to the 'DynamicConfigDatum'
  voteNftTokenName :: TokenName <-
    liftContractM "Could not make voteNft token name" $ mkTokenName
      "vote_pass"
  let
    -- The symbol of the vote 'multiplier' token
    fungibleSymbol :: CurrencySymbol
    fungibleSymbol = configDatum # unwrap # _.voteFungibleCurrencySymbol

    -- The token name of the vote 'multiplier' token
    fungibleTokenName :: TokenName
    fungibleTokenName = configDatum # unwrap # _.voteFungibleTokenName

    -- The amount of fungible tokens this user possesses
    fungibleAmount :: BigInt
    fungibleAmount = countOfTokenInValue fungibleSymbol voteInfo.value

    -- Create the fungible value based on the amount the user possesses
    fungibleToken :: Value
    fungibleToken
      | fungibleAmount == zero = mempty
      | otherwise = Value.singleton fungibleSymbol
          fungibleTokenName
          fungibleAmount

    -- The symbol of the vote 'pass'
    -- A user requires this token in order to vote on a proposal
    voteNftSymbol :: CurrencySymbol
    voteNftSymbol = configDatum # unwrap # _.voteNft

    -- The vote 'pass' token with the 'voteNftSymbol'
    voteNftPass :: Value
    voteNftPass = Value.singleton voteNftSymbol voteNftTokenName one

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
        -- ^ The script requires the tx to be signed by the vote owner
        , Constraints.mustPayToPubKey voteOwnerKey
            (voteNftPass <> fungibleToken)
        -- ^ Pay the vote 'pass' back to the owner, and the fungibleTokens if any
        , configInfo.constraints
        , voteInfo.constraints
        ]

  txHash <- submitTxFromConstraints lookups constraints

  pure txHash

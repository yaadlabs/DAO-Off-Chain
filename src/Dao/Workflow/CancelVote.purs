{-|
Module: Dao.Workflow.CancelVote
Description: Contract for cancelling a vote on a proposal
-}
module Dao.Workflow.CancelVote (cancelVote) where

import Cardano.ToData (toData)
import Cardano.Types
  ( AssetName
  , BigNum
  , Mint
  , PlutusScript
  , RedeemerDatum
  , ScriptHash
  , TransactionHash
  , Value
  )
import Cardano.Types.BigNum (one, zero) as BigNum
import Cardano.Types.Int (negate, one) as CTInt
import Cardano.Types.Mint (singleton) as Mint
import Cardano.Types.Value (empty, singleton) as Value
import Contract.Address (PaymentPubKeyHash)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.Prelude
  ( bind
  , discard
  , mconcat
  , otherwise
  , pure
  , (#)
  , ($)
  , (<>)
  , (==)
  )
import Contract.ScriptLookups as Lookups
import Contract.Transaction (submitTxFromConstraints)
import Contract.TxConstraints as Constraints
import Contract.Wallet (ownPaymentPubKeyHash)
import Dao.Component.Config.Params (mkValidatorConfig)
import Dao.Component.Config.Query (ConfigInfo, referenceConfigUtxo)
import Dao.Component.Vote.Params (CancelVoteParams)
import Dao.Component.Vote.Query (VoteInfo, cancelVoteUtxo)
import Dao.Scripts.Policy (unappliedVotePolicy)
import Dao.Scripts.Validator (unappliedConfigValidator, unappliedVoteValidator)
import Dao.Utils.Address (plutusAddressToPaymentPubKeyHash)
import Dao.Utils.Value (countOfTokenInValue, mkTokenName)
import Data.Newtype (unwrap, wrap)
import LambdaBuffers.ApplicationTypes.Configuration (DynamicConfigDatum)
import LambdaBuffers.ApplicationTypes.Vote
  ( VoteActionRedeemer(VoteActionRedeemer'Cancel)
  , VoteMinterActionRedeemer(VoteMinterActionRedeemer'Burn)
  )
import Partial.Unsafe (unsafePartial)

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
  appliedVotePolicy :: PlutusScript <- unappliedVotePolicy validatorConfig
  appliedVoteValidator :: PlutusScript <- unappliedVoteValidator
    validatorConfig
  appliedConfigValidator :: PlutusScript <- unappliedConfigValidator
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
    voteSymbol :: ScriptHash
    voteSymbol = configDatum # unwrap # _.voteCurrencySymbol

    -- The token name for the token created with the 'voteSymbol'
    voteTokenName :: AssetName
    voteTokenName = configDatum # unwrap # _.voteTokenName

    -- We need to burn the vote token created and sent to
    -- the user when they voted on the proposal
    burnVoteNft :: Mint
    burnVoteNft = Mint.singleton voteSymbol voteTokenName
      (CTInt.negate CTInt.one)

    -- The 'votePolicy' minting policy takes two possible redeemers, Mint or Burn
    -- In this case we wish to burn the token we minted when voting on the proposal
    burnVoteRedeemer :: RedeemerDatum
    burnVoteRedeemer = wrap $ toData VoteMinterActionRedeemer'Burn

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
      $ plutusAddressToPaymentPubKeyHash
      $ voteInfo.datum
      # unwrap
      # _.voteOwner

  -- TODO: Add this field to the 'DynamicConfigDatum'
  voteNftTokenName :: AssetName <-
    liftContractM "Could not make voteNft token name" $ mkTokenName
      "vote_pass"
  let
    -- The symbol of the vote 'multiplier' token
    fungibleSymbol :: ScriptHash
    fungibleSymbol = configDatum # unwrap # _.voteFungibleCurrencySymbol

    -- The token name of the vote 'multiplier' token
    fungibleTokenName :: AssetName
    fungibleTokenName = configDatum # unwrap # _.voteFungibleTokenName

    -- The amount of fungible tokens this user possesses
    fungibleAmount :: BigNum
    fungibleAmount = countOfTokenInValue fungibleSymbol voteInfo.value

    -- Create the fungible value based on the amount the user possesses
    fungibleToken :: Value
    fungibleToken
      | fungibleAmount == BigNum.zero = Value.empty
      | otherwise = Value.singleton fungibleSymbol
          fungibleTokenName
          fungibleAmount

    -- The symbol of the vote 'pass'
    -- A user requires this token in order to vote on a proposal
    voteNftSymbol :: ScriptHash
    voteNftSymbol = configDatum # unwrap # _.voteNft

    -- The vote 'pass' token with the 'voteNftSymbol'
    voteNftPass :: Value
    voteNftPass = Value.singleton voteNftSymbol voteNftTokenName BigNum.one

    lookups :: Lookups.ScriptLookups
    lookups =
      mconcat
        [ Lookups.plutusMintingPolicy appliedVotePolicy
        , voteInfo.lookups
        , configInfo.lookups
        ]

    constraints :: Constraints.TxConstraints
    constraints =
      mconcat
        [ Constraints.mustMintValueWithRedeemer burnVoteRedeemer burnVoteNft
        , Constraints.mustBeSignedBy voteOwnerKey
        -- ^ The script requires the tx to be signed by the vote owner
        , Constraints.mustPayToPubKey voteOwnerKey $
            unsafePartial -- FIXME: unsafe
              (voteNftPass <> fungibleToken)
        -- ^ Pay the vote 'pass' back to the owner, and the fungibleTokens if any
        , configInfo.constraints
        , voteInfo.constraints
        ]

  txHash <- submitTxFromConstraints lookups constraints

  pure txHash

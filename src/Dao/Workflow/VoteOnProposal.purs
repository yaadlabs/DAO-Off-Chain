{-|
Module: Dao.Workflow.VoteOnProposal
Description: Contract for voting on a proposal
-}
module Dao.Workflow.VoteOnProposal
  ( VoteOnProposalResult(..)
  , voteOnProposal
  ) where

import Contract.Address (Address)
import Contract.Chain (waitNSlots)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedM)
import Contract.Numeric.Natural (fromInt') as Natural
import Contract.PlutusData (Datum(Datum), Redeemer(Redeemer), toData)
import Contract.Prelude
  ( type (/\)
  , bind
  , discard
  , mconcat
  , mempty
  , one
  , pure
  , unwrap
  , void
  , (#)
  , ($)
  , (*)
  , (/\)
  , (<>)
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts
  ( MintingPolicy
  , Validator
  , ValidatorHash(ValidatorHash)
  , validatorHash
  )
import Contract.Time (POSIXTime(POSIXTime))
import Contract.Transaction
  ( TransactionHash
  , submitTxFromConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , Value
  , scriptCurrencySymbol
  )
import Contract.Value (singleton) as Value
import Contract.Wallet (ownPaymentPubKeyHash)
import Dao.Component.Config.Query (ConfigInfo, referenceConfigUtxo)
import Dao.Component.Tally.Query (TallyInfo, referenceTallyUtxo)
import Dao.Component.Vote.Params (VoteOnProposalParams)
import Dao.Component.Vote.Query (spendFungibleUtxo, spendVoteNftUtxo)
import Dao.Scripts.Policy.Vote (unappliedVotePolicy)
import Dao.Scripts.Validator.Config (unappliedConfigValidator)
import Dao.Scripts.Validator.Tally (unappliedTallyValidator)
import Dao.Utils.Address (paymentPubKeyHashToAddress)
import Dao.Utils.Query (getAllWalletUtxos)
import Dao.Utils.Time (mkOnchainTimeRange, mkValidityRange, oneMinute)
import Data.Maybe (Maybe(Just, Nothing))
import JS.BigInt (fromInt)
import LambdaBuffers.ApplicationTypes.Configuration (DynamicConfigDatum)
import LambdaBuffers.ApplicationTypes.Vote
  ( VoteDatum(VoteDatum)
  , VoteMinterActionRedeemer(VoteMinterActionRedeemer'Mint)
  )
import ScriptArguments.Types
  ( ValidatorParams(ValidatorParams)
  )

-- | Vote result
newtype VoteOnProposalResult = VoteOnProposalResult
  { txHash :: TransactionHash
  , symbol :: CurrencySymbol
  }

-- | Contract for voting on a specific proposal
voteOnProposal ::
  VoteOnProposalParams ->
  Contract VoteOnProposalResult
voteOnProposal params' = do
  logInfo' "Entering voteOnProposal transaction"

  let params = params' # unwrap

  -- Make the scripts
  let
    validatorConfig = ValidatorParams
      { vpConfigSymbol: params.configSymbol
      , vpConfigTokenName: params.configTokenName
      }

  appliedTallyValidator :: Validator <- unappliedTallyValidator
    validatorConfig
  appliedConfigValidator :: Validator <- unappliedConfigValidator
    validatorConfig
  appliedVotePolicy :: MintingPolicy <- unappliedVotePolicy validatorConfig

  -- Query the UTXOs
  configInfo :: ConfigInfo <- referenceConfigUtxo params.configSymbol
    appliedConfigValidator
  tallyInfo :: TallyInfo <- referenceTallyUtxo params.tallySymbol
    params.proposalTokenName
    appliedTallyValidator

  let
    -- The main config referenced at the config UTXO
    configDatum :: DynamicConfigDatum
    configDatum = configInfo.datum

    -- Symbol of the vote 'pass' token (required to vote on a proposal)
    voteNftSymbol :: CurrencySymbol
    voteNftSymbol = configDatum # unwrap # _.voteNft

    fungibleSymbol :: CurrencySymbol
    fungibleSymbol = configDatum # unwrap # _.voteFungibleCurrencySymbol

    fungibleTokenName :: TokenName
    fungibleTokenName = configDatum # unwrap # _.voteFungibleTokenName

  -- Make the on-chain time range
  timeRange <- mkValidityRange (POSIXTime $ fromInt $ 5 * oneMinute)
  onchainTimeRange <- mkOnchainTimeRange timeRange

  -- Hack to work around Ogmios submitted too early error (in Plutip test)
  -- TODO: Find better solution
  void $ waitNSlots (Natural.fromInt' 10)

  -- Get the UTXOs at user's address
  userUtxos <- getAllWalletUtxos

  -- Look for vote tokens at the user's wallet,
  -- the required 'voteNft' and potentially 'fungible' multiplier tokens,
  -- get the constraints and lookups to spend this UTXO if found.
  voteNftInfo <- spendVoteNftUtxo voteNftSymbol userUtxos

  fungibleInfo <- spendFungibleUtxo fungibleSymbol voteNftSymbol
    fungibleTokenName
    userUtxos

  ownPaymentPkh <- liftedM "Could not get own payment pkh" ownPaymentPubKeyHash
  let
    -- The 'voteOwner' field of the 'VoteDatum' must correspond to the
    -- address of the wallet executing this transaction
    ownerAddress :: Address
    ownerAddress = paymentPubKeyHashToAddress ownPaymentPkh

    -- The datum includes the user's key, the type of proposal
    -- and whether the user is voting for or against the proposal
    voteDatum :: VoteDatum
    voteDatum = VoteDatum
      { proposalTokenName: params.proposalTokenName
      , direction: params.voteDirection
      , returnAda: params.returnAda
      , voteOwner: ownerAddress
      }

    -- The 'voteSymbol' is the symbol of the 'votePolicy'
    -- used when a user votes on a proposal
    voteSymbol :: CurrencySymbol
    voteSymbol = configDatum # unwrap # _.voteCurrencySymbol

    -- The token name for the token created with the 'voteSymbol'
    voteTokenName :: TokenName
    voteTokenName = configDatum # unwrap # _.voteTokenName

    -- The vote value to be minted
    voteValue :: Value
    voteValue = Value.singleton voteSymbol voteTokenName one

    -- The value to be paid to the script
    -- Consists of the vote value, voteNft value, and maybe a fungible value
    valueToPayToScript :: Value
    valueToPayToScript = case fungibleInfo of
      Just fungibleInfo' ->
        (voteValue <> voteNftInfo.value <> fungibleInfo'.value)
      Nothing -> (voteValue <> voteNftInfo.value)

    -- The 'votePolicy' minting policy takes two possible redeemers, Mint or Burn
    -- In this case we wish to mint a vote token in order to vote on the proposal
    votePolicyRedeemer :: Redeemer
    votePolicyRedeemer = Redeemer $ toData VoteMinterActionRedeemer'Mint

    -- We require the hash in order to pay to the vote validator script
    voteValidatorHash :: ValidatorHash
    voteValidatorHash = ValidatorHash $ configDatum # unwrap # _.voteValidator

    fungibleLookups :: Lookups.ScriptLookups
    fungibleLookups = case fungibleInfo of
      Just fungibleInfo' -> fungibleInfo'.lookups
      Nothing -> mempty

    lookups :: Lookups.ScriptLookups
    lookups =
      mconcat
        [ Lookups.mintingPolicy appliedVotePolicy
        , configInfo.lookups
        , tallyInfo.lookups
        , voteNftInfo.lookups
        , fungibleLookups
        ]

    fungibleConstraints :: Constraints.TxConstraints
    fungibleConstraints = case fungibleInfo of
      Just fungibleInfo' -> fungibleInfo'.constraints
      Nothing -> mempty

    constraints :: Constraints.TxConstraints
    constraints =
      mconcat
        [ Constraints.mustMintValueWithRedeemer votePolicyRedeemer voteValue
        , Constraints.mustPayToScript
            voteValidatorHash
            (Datum $ toData voteDatum)
            Constraints.DatumInline
            valueToPayToScript
        -- ^ We send the 'VoteDatum' along with the relevant vote
        -- tokens to a UTXO at the 'vote validator' script
        , Constraints.mustValidateIn onchainTimeRange
        -- ^ A time-range is required by the on-chain script in
        -- order to ensure that we are still within the voting period
        , configInfo.constraints
        , tallyInfo.constraints
        , voteNftInfo.constraints
        , fungibleConstraints
        ]

  txHash <- submitTxFromConstraints lookups constraints

  pure $ VoteOnProposalResult { txHash, symbol: voteSymbol }

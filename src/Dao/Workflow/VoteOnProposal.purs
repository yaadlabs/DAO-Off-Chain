{-|
Module: Dao.Workflow.VoteOnProposal
Description: Contract for voting on a proposal
-}
module Dao.Workflow.VoteOnProposal
  ( VoteOnProposalResult(..)
  , voteOnProposal
  ) where

import Cardano.Plutus.Types.Address (Address) as Plutus
import Cardano.Plutus.Types.Address (pubKeyHashAddress)
import Cardano.ToData (toData)
import Cardano.Types
  ( AssetName
  , PlutusScript
  , RedeemerDatum
  , ScriptHash
  , TransactionHash
  , Value
  )
import Cardano.Types.BigNum (fromInt, one) as BigNum
import Cardano.Types.Mint (fromMultiAsset) as Mint
import Cardano.Types.Value (getMultiAsset, singleton) as Value
import Contract.Chain (waitNSlots)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedM)
import Contract.Prelude
  ( bind
  , discard
  , mconcat
  , mempty
  , pure
  , show
  , unwrap
  , void
  , (#)
  , ($)
  , (*)
  , (<>)
  )
import Contract.ScriptLookups as Lookups
import Contract.Time (POSIXTime(POSIXTime))
import Contract.Transaction (submitTxFromConstraints)
import Contract.TxConstraints as Constraints
import Contract.Wallet (ownPaymentPubKeyHash)
import Dao.Component.Config.Query (ConfigInfo, referenceConfigUtxo)
import Dao.Component.Tally.Query (TallyInfo, referenceTallyUtxo)
import Dao.Component.Vote.Params (VoteOnProposalParams)
import Dao.Component.Vote.Query (spendFungibleUtxo, spendVoteNftUtxo)
import Dao.Scripts.Policy (unappliedVotePolicy)
import Dao.Scripts.Validator (unappliedConfigValidator, unappliedTallyValidator)
import Dao.Utils.Query (getAllWalletUtxos)
import Dao.Utils.Time (mkOnchainTimeRange, mkValidityRange, oneMinute)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (wrap)
import Debug (traceM)
import JS.BigInt (fromInt)
import LambdaBuffers.ApplicationTypes.Configuration (DynamicConfigDatum)
import LambdaBuffers.ApplicationTypes.Vote
  ( VoteDatum(VoteDatum)
  , VoteMinterActionRedeemer(VoteMinterActionRedeemer'Mint)
  )
import Partial.Unsafe (unsafePartial)
import ScriptArguments.Types (ValidatorParams(ValidatorParams))

-- | Vote result
newtype VoteOnProposalResult = VoteOnProposalResult
  { txHash :: TransactionHash
  , symbol :: ScriptHash
  }

-- | Contract for voting on a specific proposal
voteOnProposal ::
  VoteOnProposalParams ->
  Contract VoteOnProposalResult
voteOnProposal params' = do
  logInfo' "Entering voteOnProposal transaction"

  let params = params' # unwrap

  logInfo' $ "VoteOnProposalParams: " <> show params

  -- Make the scripts
  let
    validatorConfig = ValidatorParams
      { vpConfigSymbol: params.configSymbol
      , vpConfigTokenName: params.configTokenName
      }

  appliedTallyValidator :: PlutusScript <- unappliedTallyValidator
    validatorConfig
  appliedConfigValidator :: PlutusScript <- unappliedConfigValidator
    validatorConfig
  appliedVotePolicy :: PlutusScript <- unappliedVotePolicy validatorConfig

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
    voteNftSymbol :: ScriptHash
    voteNftSymbol = configDatum # unwrap # _.voteNft

    fungibleSymbol :: ScriptHash
    fungibleSymbol = configDatum # unwrap # _.voteFungibleCurrencySymbol

    fungibleTokenName :: AssetName
    fungibleTokenName = configDatum # unwrap # _.voteFungibleTokenName

  -- Make the on-chain time range
  timeRange <- mkValidityRange (POSIXTime $ fromInt $ 5 * oneMinute)
  onchainTimeRange <- mkOnchainTimeRange timeRange
  traceM $ "PROPOSAL END TIME: " <> show
    (unwrap tallyInfo.datum).proposalEndTime
  traceM $ " TX VALID RANGE: " <> show onchainTimeRange

  -- Hack to work around Ogmios submitted too early error (in Plutip test)
  -- TODO: Find better solution
  void $ waitNSlots $ BigNum.fromInt 10

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
    ownerAddress :: Plutus.Address
    ownerAddress = pubKeyHashAddress (wrap $ wrap $ unwrap ownPaymentPkh)
      Nothing

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
    voteSymbol :: ScriptHash
    voteSymbol = configDatum # unwrap # _.voteCurrencySymbol

    -- The token name for the token created with the 'voteSymbol'
    voteTokenName :: AssetName
    voteTokenName = configDatum # unwrap # _.voteTokenName

    -- The vote value to be minted
    voteValue :: Value
    voteValue = Value.singleton voteSymbol voteTokenName BigNum.one

    -- The value to be paid to the script
    -- Consists of the vote value, voteNft value, and maybe a fungible value
    valueToPayToScript :: Value
    valueToPayToScript =
      -- FIXME: unsafe
      unsafePartial $
        case fungibleInfo of
          Just fungibleInfo' ->
            voteValue <> voteNftInfo.value <> fungibleInfo'.value
          Nothing ->
            voteValue <> voteNftInfo.value

    -- The 'votePolicy' minting policy takes two possible redeemers, Mint or Burn
    -- In this case we wish to mint a vote token in order to vote on the proposal
    votePolicyRedeemer :: RedeemerDatum
    votePolicyRedeemer = wrap $ toData VoteMinterActionRedeemer'Mint

    -- We require the hash in order to pay to the vote validator script
    voteValidatorHash :: ScriptHash
    voteValidatorHash = configDatum # unwrap # _.voteValidator

    fungibleLookups :: Lookups.ScriptLookups
    fungibleLookups = case fungibleInfo of
      Just fungibleInfo' -> fungibleInfo'.lookups
      Nothing -> mempty

    lookups :: Lookups.ScriptLookups
    lookups =
      mconcat
        [ Lookups.plutusMintingPolicy appliedVotePolicy
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
        [ Constraints.mustMintValueWithRedeemer votePolicyRedeemer $
            Mint.fromMultiAsset (Value.getMultiAsset voteValue)
        , Constraints.mustPayToScript
            voteValidatorHash
            (toData voteDatum)
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

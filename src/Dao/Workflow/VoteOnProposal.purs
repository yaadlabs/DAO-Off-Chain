{-|
Module: Dao.Workflow.VoteOnProposal
Description: Contract for voting on a proposal
-}
module Dao.Workflow.VoteOnProposal (voteOnProposal) where

import Contract.Address (Address)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedM)
import Contract.PlutusData (Datum(Datum), Redeemer(Redeemer), toData)
import Contract.Prelude
  ( type (/\)
  , bind
  , discard
  , mconcat
  , one
  , pure
  , unwrap
  , (#)
  , ($)
  , (*)
  , (/\)
  , (<>)
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy, Validator, ValidatorHash, validatorHash)
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
import Dao.Component.Vote.Query (spendVoteNftUtxo)
import Dao.Scripts.Policy.VotePolicy (unappliedVotePolicyDebug)
import Dao.Scripts.Validator.ConfigValidator (unappliedConfigValidatorDebug)
import Dao.Scripts.Validator.TallyValidator (unappliedTallyValidatorDebug)
import Dao.Scripts.Validator.VoteValidator (unappliedVoteValidatorDebug)
import Dao.Utils.Address (paymentPubKeyHashToAddress)
import Dao.Utils.Query (getAllWalletUtxos)
import Dao.Utils.Time (mkOnchainTimeRange, mkValidityRange, oneMinute)
import JS.BigInt (fromInt)
import LambdaBuffers.ApplicationTypes.Arguments
  ( ConfigurationValidatorConfig(ConfigurationValidatorConfig)
  )
import LambdaBuffers.ApplicationTypes.Vote
  ( VoteDatum(VoteDatum)
  , VoteMinterActionRedeemer(VoteMinterActionRedeemer'Mint)
  )

-- | Contract for voting on a specific proposal
voteOnProposal ::
  VoteOnProposalParams ->
  Contract (TransactionHash /\ CurrencySymbol)
voteOnProposal params' = do
  logInfo' "Entering voteOnProposal transaction"

  let params = params' # unwrap

  -- Make the scripts
  let
    validatorConfig = ConfigurationValidatorConfig
      { cvcConfigNftCurrencySymbol: params.configSymbol
      , cvcConfigNftTokenName: params.configTokenName
      }

  appliedTallyValidator :: Validator <- unappliedTallyValidatorDebug
    validatorConfig
  appliedConfigValidator :: Validator <- unappliedConfigValidatorDebug
    validatorConfig
  appliedVotePolicy :: MintingPolicy <- unappliedVotePolicyDebug validatorConfig
  appliedVoteValidator :: Validator <- unappliedVoteValidatorDebug
    validatorConfig

  -- Query the UTXOs
  configInfo :: ConfigInfo <- referenceConfigUtxo params.configSymbol
    appliedConfigValidator
  tallyInfo :: TallyInfo <- referenceTallyUtxo params.tallySymbol
    appliedTallyValidator

  -- Make the on-chain time range
  timeRange <- mkValidityRange (POSIXTime $ fromInt $ 5 * oneMinute)
  onchainTimeRange <- mkOnchainTimeRange timeRange

  -- Get the UTXOs at user's address
  userUtxos <- getAllWalletUtxos

  -- Check if the user has a 'voteNft' token,
  -- which is required in order to vote on a proposal
  -- TODO: Spend it properly, now just finding it
  voteNftToken <- spendVoteNftUtxo params.voteNftSymbol userUtxos

  ownPaymentPkh <- liftedM "Could not get own payment pkh" ownPaymentPubKeyHash
  let
    ownerAddress :: Address
    ownerAddress = paymentPubKeyHashToAddress ownPaymentPkh

    voteDatum :: VoteDatum
    voteDatum = VoteDatum
      { proposalTokenName: params.proposalTokenName
      , direction: params.voteDirection
      , returnAda: params.returnAda
      , voteOwner: ownerAddress
      }

    voteSymbol :: CurrencySymbol
    voteSymbol = scriptCurrencySymbol appliedVotePolicy

    voteValue :: Value
    voteValue = Value.singleton voteSymbol params.voteTokenName one

    votePolicyRedeemer :: Redeemer
    votePolicyRedeemer = Redeemer $ toData VoteMinterActionRedeemer'Mint

    voteValidatorHash :: ValidatorHash
    voteValidatorHash = validatorHash appliedVoteValidator

    lookups :: Lookups.ScriptLookups
    lookups =
      mconcat
        [ Lookups.mintingPolicy appliedVotePolicy
        , configInfo.lookups
        , tallyInfo.lookups
        ]

    constraints :: Constraints.TxConstraints
    constraints =
      mconcat
        [ Constraints.mustMintValueWithRedeemer votePolicyRedeemer voteValue
        , Constraints.mustPayToScript
            voteValidatorHash
            (Datum $ toData voteDatum)
            Constraints.DatumInline
            (voteValue <> voteNftToken)
        , Constraints.mustValidateIn onchainTimeRange
        , configInfo.constraints
        , tallyInfo.constraints
        ]

  txHash <- submitTxFromConstraints lookups constraints

  pure (txHash /\ voteSymbol)

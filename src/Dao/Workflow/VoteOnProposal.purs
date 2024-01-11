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
  , ($)
  , (*)
  , (/\)
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
import Dao.Component.Vote.Params (VoteParams)
import Dao.Utils.Address (paymentPubKeyHashToAddress)
import Dao.Utils.Time (mkOnchainTimeRange, mkValidityRange, oneMinute)
import JS.BigInt (fromInt)
import LambdaBuffers.ApplicationTypes.Arguments
  ( ConfigurationValidatorConfig(ConfigurationValidatorConfig)
  )
import LambdaBuffers.ApplicationTypes.Vote
  ( VoteDatum(VoteDatum)
  , VoteMinterActionRedeemer(VoteMinterActionRedeemer'Mint)
  )
import Scripts.ConfigValidator (unappliedConfigValidatorDebug)
import Scripts.TallyValidator (unappliedTallyValidatorDebug)
import Scripts.VotePolicy (unappliedVotePolicyDebug)
import Scripts.VoteValidator (unappliedVoteValidatorDebug)

-- | Contract for voting on a specific proposal
voteOnProposal ::
  VoteParams ->
  Contract (TransactionHash /\ CurrencySymbol)
voteOnProposal voteParams = do
  logInfo' "Entering voteOnProposal transaction"

  -- Make the scripts
  let
    validatorConfig = ConfigurationValidatorConfig
      { cvcConfigNftCurrencySymbol: voteParams.configSymbol
      , cvcConfigNftTokenName: voteParams.configTokenName
      }

  appliedTallyValidator :: Validator <- unappliedTallyValidatorDebug
    validatorConfig
  appliedConfigValidator :: Validator <- unappliedConfigValidatorDebug
    validatorConfig
  appliedVotePolicy :: MintingPolicy <- unappliedVotePolicyDebug validatorConfig
  appliedVoteValidator :: Validator <- unappliedVoteValidatorDebug
    validatorConfig

  -- Query the UTXOs
  configInfo :: ConfigInfo <- referenceConfigUtxo voteParams.configSymbol
    appliedConfigValidator
  tallyInfo :: TallyInfo <- referenceTallyUtxo voteParams.tallySymbol
    appliedTallyValidator

  -- Make the on-chain time range
  timeRange <- mkValidityRange (POSIXTime $ fromInt $ 5 * oneMinute)
  onchainTimeRange <- mkOnchainTimeRange timeRange

  ownPaymentPkh <- liftedM "Could not get own payment pkh" ownPaymentPubKeyHash
  let
    ownerAddress :: Address
    ownerAddress = paymentPubKeyHashToAddress ownPaymentPkh

    voteDatum :: VoteDatum
    voteDatum = VoteDatum
      { proposalTokenName: voteParams.proposalTokenName
      , direction: voteParams.voteDirection
      , returnAda: voteParams.returnAda
      , voteOwner: ownerAddress
      }

    voteSymbol :: CurrencySymbol
    voteSymbol = scriptCurrencySymbol appliedVotePolicy

    voteNft :: Value
    voteNft = Value.singleton voteParams.voteSymbol voteParams.voteTokenName one

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
        [ Constraints.mustMintValueWithRedeemer votePolicyRedeemer voteNft
        , Constraints.mustPayToScript
            voteValidatorHash
            (Datum $ toData voteDatum)
            Constraints.DatumInline
            voteNft
        , Constraints.mustValidateIn onchainTimeRange
        , configInfo.constraints
        , tallyInfo.constraints
        ]

  txHash <- submitTxFromConstraints lookups constraints

  pure (txHash /\ voteSymbol)

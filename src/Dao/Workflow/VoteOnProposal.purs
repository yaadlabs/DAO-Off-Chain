{-|
Module: Dao.Workflow.VoteOnProposal
Description: Contract for voting on a proposal
-}
module Dao.Workflow.VoteOnProposal (voteOnProposal) where

import Contract.Log (logInfo')
import Contract.Monad (Contract)
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
import Dao.Component.Config.Query (ConfigInfo, referenceConfigUtxo)
import Dao.Component.Tally.Query (TallyInfo, referenceTallyUtxo)
import Dao.Utils.Time (mkOnchainTimeRange, mkValidityRange, oneMinute)
import JS.BigInt (fromInt)
import LambdaBuffers.ApplicationTypes.Arguments
  ( ConfigurationValidatorConfig(ConfigurationValidatorConfig)
  )
import LambdaBuffers.ApplicationTypes.Vote
  ( VoteDatum
  , VoteMinterActionRedeemer(VoteMinterActionRedeemer'Mint)
  )
import Scripts.ConfigValidator (unappliedConfigValidatorDebug)
import Scripts.TallyValidator (unappliedTallyValidator)
import Scripts.VotePolicy (unappliedVotePolicy)
import Scripts.VoteValidator (unappliedVoteValidator)

type VoteInfo = { voteSymbol :: CurrencySymbol, voteTokenName :: TokenName }

-- | Contract for voting on a specific proposal
voteOnProposal ::
  CurrencySymbol ->
  CurrencySymbol ->
  TokenName ->
  VoteInfo ->
  VoteDatum ->
  Contract (TransactionHash /\ CurrencySymbol)
voteOnProposal configSymbol tallySymbol configTokenName voteInfo voteDatum = do
  logInfo' "Entering voteOnProposal transaction"

  -- Make the scripts
  let
    validatorConfig = ConfigurationValidatorConfig
      { cvcConfigNftCurrencySymbol: configSymbol
      , cvcConfigNftTokenName: configTokenName
      }

  appliedTallyValidator :: Validator <- unappliedTallyValidator validatorConfig
  appliedConfigValidator :: Validator <- unappliedConfigValidatorDebug
    validatorConfig
  appliedVotePolicy :: MintingPolicy <- unappliedVotePolicy validatorConfig
  appliedVoteValidator :: Validator <- unappliedVoteValidator validatorConfig

  -- Query the UTXOs
  configInfo :: ConfigInfo <- referenceConfigUtxo configSymbol
    appliedConfigValidator
  tallyInfo :: TallyInfo <- referenceTallyUtxo tallySymbol appliedTallyValidator

  -- Make the on-chain time range
  timeRange <- mkValidityRange (POSIXTime $ fromInt $ 5 * oneMinute)
  onchainTimeRange <- mkOnchainTimeRange timeRange

  let
    voteSymbol :: CurrencySymbol
    voteSymbol = scriptCurrencySymbol appliedVotePolicy

    voteNft :: Value
    voteNft = Value.singleton voteInfo.voteSymbol voteInfo.voteTokenName one

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

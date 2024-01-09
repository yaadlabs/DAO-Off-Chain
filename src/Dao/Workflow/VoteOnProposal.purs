{-|
Module: Dao.Workflow.VoteOnProposal
Description: Contract for voting on a proposal
-}
module Dao.Workflow.VoteOnProposal (voteOnProposal) where

import Contract.Address (scriptHashAddress)
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
  , TransactionInput
  , TransactionOutputWithRefScript
  , submitTxFromConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Utxos (UtxoMap, utxosAt)
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , Value
  , scriptCurrencySymbol
  )
import Contract.Value (singleton) as Value
import Dao.Utils.Query
  ( QueryType(Reference)
  , UtxoInfo
  , findUtxoBySymbol
  , findUtxoByValue
  )
import Dao.Utils.Time (mkOnchainTimeRange, mkValidityRange, oneMinute)
import Data.Maybe (Maybe(Nothing))
import JS.BigInt (fromInt)
-- import ScriptArguments.Types
--   ( ConfigurationValidatorConfig(ConfigurationValidatorConfig)
--   )
import LambdaBuffers.ApplicationTypes.Arguments
  ( ConfigurationValidatorConfig(ConfigurationValidatorConfig)
  )
import LambdaBuffers.ApplicationTypes.Configuration (DynamicConfigDatum)
import LambdaBuffers.ApplicationTypes.Tally (TallyStateDatum)
import LambdaBuffers.ApplicationTypes.Vote
  ( VoteDatum
  , VoteMinterActionRedeemer(VoteMinterActionRedeemer'Mint)
  )
import Scripts.ConfigValidator (unappliedConfigValidator)
import Scripts.TallyValidator (unappliedTallyValidator)
import Scripts.VotePolicy (unappliedVotePolicy)
import Scripts.VoteValidator (unappliedVoteValidator)
import Type.Proxy (Proxy(Proxy))

type VoteInfo = { voteSymbol :: CurrencySymbol, voteTokenName :: TokenName }

voteOnProposal ::
  CurrencySymbol ->
  CurrencySymbol ->
  TokenName ->
  VoteInfo ->
  VoteDatum ->
  Contract (TransactionHash /\ CurrencySymbol)
voteOnProposal configSymbol tallySymbol configTokenName voteInfo voteDatum = do
  logInfo' "Entering voteOnProposal transaction"

  let
    validatorConfig = ConfigurationValidatorConfig
      { cvcConfigNftCurrencySymbol: configSymbol
      , cvcConfigNftTokenName: configTokenName
      }

  appliedTallyValidator :: Validator <- unappliedTallyValidator validatorConfig
  appliedConfigValidator :: Validator <- unappliedConfigValidator
    validatorConfig

  configInfo <- findUtxoBySymbol (Proxy :: Proxy DynamicConfigDatum) Reference
    configSymbol
    appliedConfigValidator
  tallyInfo <- findUtxoBySymbol (Proxy :: Proxy TallyStateDatum) Reference
    tallySymbol
    appliedTallyValidator

  appliedVotePolicy :: MintingPolicy <- unappliedVotePolicy validatorConfig
  appliedVoteValidator :: Validator <- unappliedVoteValidator validatorConfig

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

{-|
Module: Dao.Workflow.CancelVote
Description: Contract for cancelling a vote on a proposal
-}
module Dao.Workflow.CancelVote (cancelVote) where

import Contract.Address
  ( PaymentPubKeyHash
  , scriptHashAddress
  )
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.PlutusData
  ( Datum
  , Redeemer(Redeemer)
  , fromData
  , toData
  )
import Contract.Prelude
  ( type (/\)
  , bind
  , discard
  , mconcat
  , negate
  , one
  , pure
  , (#)
  , ($)
  , (/\)
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy, Validator, validatorHash)
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
import Dao.Utils.Address (addressToPaymentPubKeyHash)
import Dao.Utils.Datum (getInlineDatumFromTxOutWithRefScript)
import Dao.Utils.Query (findUtxoByValue)
import Data.Map as Map
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (unwrap)
import LambdaBuffers.ApplicationTypes.Arguments
  ( ConfigurationValidatorConfig(ConfigurationValidatorConfig)
  )
import LambdaBuffers.ApplicationTypes.Vote
  ( VoteActionRedeemer(VoteActionRedeemer'Cancel)
  , VoteDatum
  , VoteMinterActionRedeemer(VoteMinterActionRedeemer'Burn)
  )
import Scripts.ConfigValidator (unappliedConfigValidator)
import Scripts.VotePolicy (unappliedVotePolicy)
import Scripts.VoteValidator (unappliedVoteValidator)

type VoteInfo = { voteSymbol :: CurrencySymbol, voteTokenName :: TokenName }

cancelVote ::
  ConfigurationValidatorConfig ->
  VoteInfo ->
  Contract TransactionHash
cancelVote validatorConfig voteInfo = do
  logInfo' "Entering cancelVote transaction"

  appliedVotePolicy :: MintingPolicy <- unappliedVotePolicy validatorConfig
  appliedVoteValidator :: Validator <- unappliedVoteValidator validatorConfig
  appliedConfigValidator :: Validator <- unappliedConfigValidator
    validatorConfig

  let
    configValidatorAddress = scriptHashAddress
      (validatorHash appliedConfigValidator)
      Nothing
  configValidatorUtxoMap <- utxosAt configValidatorAddress

  let
    voteValidatorAddress = scriptHashAddress
      (validatorHash appliedVoteValidator)
      Nothing
  voteValidatorUtxoMap <- utxosAt voteValidatorAddress

  (configUtxoTxInput /\ _) <-
    liftedM "Could not find config UTXO" $ getConfigUtxo validatorConfig
      configValidatorUtxoMap

  (voteUtxoTxInput /\ voteUtxoTxOutRefScript) <-
    liftedM "Could not find config UTXO" $ getVoteUtxo voteInfo
      voteValidatorUtxoMap

  voteDatum' :: Datum <-
    liftContractM "No Inline vote datum at OutputDatum" $
      getInlineDatumFromTxOutWithRefScript voteUtxoTxOutRefScript
  voteDatum :: VoteDatum <-
    liftContractM "Could not convert datum" $ fromData $ voteDatum' # unwrap

  voteOwnerKey :: PaymentPubKeyHash <-
    liftContractM "Could not convert address to key"
      $ addressToPaymentPubKeyHash
      $ voteDatum
      # unwrap
      # _.voteOwner

  let
    voteSymbol :: CurrencySymbol
    voteSymbol = scriptCurrencySymbol appliedVotePolicy

    burnVoteNft :: Value
    burnVoteNft = Value.singleton voteSymbol voteInfo.voteTokenName (negate one)

    cancelRedeemer :: Redeemer
    cancelRedeemer = Redeemer $ toData VoteActionRedeemer'Cancel

    burnVoteRedeemer :: Redeemer
    burnVoteRedeemer = Redeemer $ toData VoteMinterActionRedeemer'Burn

    lookups :: Lookups.ScriptLookups
    lookups =
      mconcat
        [ Lookups.mintingPolicy appliedVotePolicy
        , Lookups.unspentOutputs $ Map.singleton voteUtxoTxInput
            voteUtxoTxOutRefScript
        ]

    constraints :: Constraints.TxConstraints
    constraints =
      mconcat
        [ Constraints.mustMintValueWithRedeemer burnVoteRedeemer burnVoteNft
        , Constraints.mustReferenceOutput configUtxoTxInput
        , Constraints.mustSpendScriptOutput voteUtxoTxInput cancelRedeemer
        , Constraints.mustBeSignedBy voteOwnerKey
        ]

  txHash <- submitTxFromConstraints lookups constraints

  pure txHash
  where
  getConfigUtxo ::
    ConfigurationValidatorConfig ->
    UtxoMap ->
    Contract (Maybe (TransactionInput /\ TransactionOutputWithRefScript))
  getConfigUtxo
    ( ConfigurationValidatorConfig
        { cvcConfigNftCurrencySymbol, cvcConfigNftTokenName }
    ) =
    findUtxoByValue
      (Value.singleton cvcConfigNftCurrencySymbol cvcConfigNftTokenName one)

  getVoteUtxo ::
    VoteInfo ->
    UtxoMap ->
    Contract (Maybe (TransactionInput /\ TransactionOutputWithRefScript))
  getVoteUtxo ({ voteSymbol, voteTokenName }) =
    findUtxoByValue
      (Value.singleton voteSymbol voteTokenName one)

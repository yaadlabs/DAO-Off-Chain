{-|
Module: Dao.Utils.Query
Description: Query helpers
-}
module Dao.Utils.Query
  ( UtxoInfo
  , QueryType(..)
  , SpendPubKeyResult
  , getAllWalletUtxos
  , hasTokenWithSymbol
  , findScriptUtxoBySymbol
  , findScriptUtxoByToken
  , findScriptUtxoByTokenWithScriptRef
  , findScriptUtxoBySymbolAndPkhInDatumAndProposalTokenNameInDatum
  , findScriptUtxoBySymbolWithScriptRef
  ) where

import Contract.Address (PaymentPubKeyHash, scriptHashAddress)
import Contract.Log (logInfo')
import Contract.Monad
  ( Contract
  , liftContractM
  , liftedM
  , throwContractError
  )
import Contract.PlutusData
  ( class FromData
  , Datum(Datum)
  , OutputDatum(OutputDatum)
  , Redeemer
  , fromData
  )
import Contract.Prelude
  ( class Eq
  , type (/\)
  , any
  , bind
  , discard
  , mconcat
  , one
  , pure
  , show
  , (#)
  , ($)
  , (&&)
  , (/\)
  , (<>)
  , (==)
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts (Validator, validatorHash)
import Contract.Transaction
  ( TransactionInput
  , TransactionOutput
  , TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  )
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , Value
  , symbols
  , valueOf
  )
import Contract.Wallet (getWalletUtxos)
import Dao.Utils.Address (addressToPaymentPubKeyHash)
import Data.Array (filter, head)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap)
import LambdaBuffers.ApplicationTypes.Vote (VoteDatum(VoteDatum))
import Type.Proxy (Proxy(Proxy))

-- | Result of querying UTXO
type UtxoInfo (datum' :: Type) =
  { lookups :: Lookups.ScriptLookups
  , constraints :: Constraints.TxConstraints
  , datum :: datum'
  , value :: Value
  }

data QueryType = Spend | Reference

derive instance Eq QueryType

-- | Reference or spend a UTXO at script marked by an NFT with the given CurrencySymbol
findScriptUtxoBySymbol ::
  forall (datum' :: Type).
  FromData datum' =>
  Proxy datum' ->
  QueryType ->
  Redeemer ->
  CurrencySymbol ->
  Validator ->
  Contract (UtxoInfo datum')
findScriptUtxoBySymbol _ spendOrReference redeemer symbol validatorScript = do
  logInfo' "Entering findScriptUtxoBySymbol contract"

  let
    scriptAddr = scriptHashAddress (validatorHash validatorScript) Nothing
  utxos <- utxosAt scriptAddr

  (txIn /\ TransactionOutputWithRefScript txOut) ::
    (TransactionInput /\ TransactionOutputWithRefScript) <-
    liftContractM "Cannot find UTxO with NFT"
      $ head
      $ filter (hasTokenWithSymbol symbol)
      $ Map.toUnfoldable
      $ utxos

  let
    constraints :: Constraints.TxConstraints
    constraints =
      case spendOrReference of
        Spend -> Constraints.mustSpendScriptOutput txIn redeemer
        Reference -> Constraints.mustReferenceOutput txIn

    lookups :: Lookups.ScriptLookups
    lookups = mconcat
      [ Lookups.unspentOutputs $ Map.singleton txIn
          (TransactionOutputWithRefScript txOut)
      , Lookups.validator validatorScript
      ]

    value :: Value
    value = txOut.output # unwrap # _.amount

  datum :: datum' <- extractDatum (Proxy :: Proxy datum') txOut.output

  pure { datum, value, lookups, constraints }

-- | Reference or spend a UTXO at script marked by an NFT with the given CurrencySymbol
findScriptUtxoBySymbolWithScriptRef ::
  forall (datum' :: Type).
  FromData datum' =>
  Proxy datum' ->
  QueryType ->
  Redeemer ->
  CurrencySymbol ->
  Validator ->
  Constraints.InputWithScriptRef ->
  Contract (UtxoInfo datum')
findScriptUtxoBySymbolWithScriptRef
  _
  spendOrReference
  redeemer
  symbol
  validatorScript
  scriptRef = do
  logInfo' "Entering findScriptUtxoBySymbol contract"

  let
    scriptAddr = scriptHashAddress (validatorHash validatorScript) Nothing
  utxos <- utxosAt scriptAddr

  (txIn /\ TransactionOutputWithRefScript txOut) ::
    (TransactionInput /\ TransactionOutputWithRefScript) <-
    liftContractM "Cannot find UTxO with NFT"
      $ head
      $ filter (hasTokenWithSymbol symbol)
      $ Map.toUnfoldable
      $ utxos

  let
    constraints :: Constraints.TxConstraints
    constraints =
      case spendOrReference of
        Spend -> Constraints.mustSpendScriptOutputUsingScriptRef txIn redeemer
          scriptRef
        Reference -> Constraints.mustReferenceOutput txIn

    lookups :: Lookups.ScriptLookups
    lookups = mconcat
      [ Lookups.unspentOutputs $ Map.singleton txIn
          (TransactionOutputWithRefScript txOut)
      , Lookups.validator validatorScript
      ]

    value :: Value
    value = txOut.output # unwrap # _.amount

  datum :: datum' <- extractDatum (Proxy :: Proxy datum') txOut.output

  pure { datum, value, lookups, constraints }

-- | Reference or spend a UTXO at script marked by an NFT
-- | with the given 'CurrencySymbol' and 'TokenName'
findScriptUtxoByToken ::
  forall (datum' :: Type).
  FromData datum' =>
  Proxy datum' ->
  QueryType ->
  Redeemer ->
  CurrencySymbol ->
  TokenName ->
  Validator ->
  Contract (UtxoInfo datum')
findScriptUtxoByToken
  _
  spendOrReference
  redeemer
  symbol
  tokenName
  validatorScript = do
  logInfo' "Entering findScriptUtxoBySymbol contract"

  let
    scriptAddr = scriptHashAddress (validatorHash validatorScript) Nothing
  utxos <- utxosAt scriptAddr

  (txIn /\ TransactionOutputWithRefScript txOut) ::
    (TransactionInput /\ TransactionOutputWithRefScript) <-
    liftContractM "Cannot find UTxO with NFT"
      $ head
      $ filter (hasOneOfToken symbol tokenName)
      $ Map.toUnfoldable
      $ utxos

  let
    constraints :: Constraints.TxConstraints
    constraints =
      case spendOrReference of
        Spend -> Constraints.mustSpendScriptOutput txIn redeemer
        Reference -> Constraints.mustReferenceOutput txIn

    lookups :: Lookups.ScriptLookups
    lookups = mconcat
      [ Lookups.unspentOutputs $ Map.singleton txIn
          (TransactionOutputWithRefScript txOut)
      , Lookups.validator validatorScript
      ]

    value :: Value
    value = txOut.output # unwrap # _.amount

  datum :: datum' <- extractDatum (Proxy :: Proxy datum') txOut.output

  pure { datum, value, lookups, constraints }

-- | Reference or spend a UTXO at script marked by an NFT
-- | with the given 'CurrencySymbol' and 'TokenName'
findScriptUtxoByTokenWithScriptRef ::
  forall (datum' :: Type).
  FromData datum' =>
  Proxy datum' ->
  QueryType ->
  Redeemer ->
  CurrencySymbol ->
  TokenName ->
  Validator ->
  Constraints.InputWithScriptRef ->
  Contract (UtxoInfo datum')
findScriptUtxoByTokenWithScriptRef
  _
  spendOrReference
  redeemer
  symbol
  tokenName
  validatorScript
  scriptRef = do
  logInfo' "Entering findScriptUtxoBySymbol contract"

  let
    scriptAddr = scriptHashAddress (validatorHash validatorScript) Nothing
  utxos <- utxosAt scriptAddr

  (txIn /\ TransactionOutputWithRefScript txOut) ::
    (TransactionInput /\ TransactionOutputWithRefScript) <-
    liftContractM "Cannot find UTxO with NFT"
      $ head
      $ filter (hasOneOfToken symbol tokenName)
      $ Map.toUnfoldable
      $ utxos

  let
    constraints :: Constraints.TxConstraints
    constraints =
      case spendOrReference of
        Spend -> Constraints.mustSpendScriptOutputUsingScriptRef txIn redeemer
          scriptRef
        Reference -> Constraints.mustReferenceOutput txIn

    lookups :: Lookups.ScriptLookups
    lookups = mconcat
      [ Lookups.unspentOutputs $ Map.singleton txIn
          (TransactionOutputWithRefScript txOut)
      , Lookups.validator validatorScript
      ]

    value :: Value
    value = txOut.output # unwrap # _.amount

  datum :: datum' <- extractDatum (Proxy :: Proxy datum') txOut.output

  pure { datum, value, lookups, constraints }

-- | Extract the output datum or throw an error
-- | if it isn't an 'OutputDatum' or 'fromData' conversion fails
extractDatum ::
  forall (datum' :: Type).
  FromData datum' =>
  Proxy datum' ->
  TransactionOutput ->
  Contract datum'
extractDatum _ txOut =
  case txOut # unwrap # _.datum of
    OutputDatum (Datum rawInlineDatum) -> case fromData rawInlineDatum of
      Just (datum :: datum') -> pure datum
      Nothing -> throwContractError "Cannot parse datum"
    dat -> throwContractError $ "Missing inline datum, got: " <> show dat

-- | This function is called by the 'cancelVoteUtxo' function.
-- | It ensures that the 'voteUtxo' we spend has a 'VoteDatum' where the
-- | 'voteOwner' field of the datum matches the 'PaymentPubKeyHash' of the
-- | user calling the function (the wallet executing the 'cancelVote' transaction).
-- | This is to ensure that the user is cancelling their own vote and not
-- | another user's vote, in which case the 'cancelVote' transaction would
-- | fail with a 'missing signatures' error.
-- | It also checks that the 'proposalTokenName' field of the 'VoteDatum'
-- | at this UTXO matches the 'proposalTokenName' passed as an argument.
-- | This is to ensure the user's vote is not on a different proposal.
findScriptUtxoBySymbolAndPkhInDatumAndProposalTokenNameInDatum ::
  Redeemer ->
  CurrencySymbol ->
  PaymentPubKeyHash ->
  TokenName ->
  Validator ->
  Contract (UtxoInfo VoteDatum)
findScriptUtxoBySymbolAndPkhInDatumAndProposalTokenNameInDatum
  redeemer
  symbol
  userPkh
  proposalTokenName
  validatorScript = do
  logInfo' "Entering findScriptUtxoBySymbolAndPkhInDatum contract"

  let
    scriptAddr = scriptHashAddress (validatorHash validatorScript) Nothing
  utxos <- utxosAt scriptAddr

  (txIn /\ TransactionOutputWithRefScript txOut) ::
    (TransactionInput /\ TransactionOutputWithRefScript) <-
    liftContractM "Cannot find UTxO with NFT"
      $ head
      $ filter
          ( hasTokenWithSymbol symbol
              && hasPkhInVoteDatum userPkh
              &&
                hasProposalTokenNameInVoteDatum proposalTokenName
          )
      $ Map.toUnfoldable
      $ utxos

  let
    constraints :: Constraints.TxConstraints
    constraints = Constraints.mustSpendScriptOutput txIn redeemer

    lookups :: Lookups.ScriptLookups
    lookups = mconcat
      [ Lookups.unspentOutputs $ Map.singleton txIn
          (TransactionOutputWithRefScript txOut)
      , Lookups.validator validatorScript
      ]

    value :: Value
    value = txOut.output # unwrap # _.amount

  datum :: VoteDatum <- extractDatum (Proxy :: Proxy VoteDatum) txOut.output

  pure { datum, value, lookups, constraints }

-- | Check that the 'userPkh' is equivalent to
-- | the 'voteOwner' field of the 'VoteDatum'
hasPkhInVoteDatum ::
  PaymentPubKeyHash ->
  (TransactionInput /\ TransactionOutputWithRefScript) ->
  Boolean
hasPkhInVoteDatum userPkh (_ /\ TransactionOutputWithRefScript txOut) =
  case txOut.output # unwrap # _.datum of
    OutputDatum (Datum rawInlineDatum) -> case fromData rawInlineDatum of
      Just (datum :: VoteDatum) ->
        case
          addressToPaymentPubKeyHash $ datum # unwrap # _.voteOwner
          of
          Just datumPkh -> datumPkh == userPkh
          _ -> false
      _ -> false
    _ -> false

-- | Check that the 'proposalTokenName' passed as an argument is equivalent
-- | to the 'proposalTokenName' field of the 'VoteDatum'
hasProposalTokenNameInVoteDatum ::
  TokenName ->
  (TransactionInput /\ TransactionOutputWithRefScript) ->
  Boolean
hasProposalTokenNameInVoteDatum
  proposalTokenName
  (_ /\ TransactionOutputWithRefScript txOut) =
  case txOut.output # unwrap # _.datum of
    OutputDatum (Datum rawInlineDatum) -> case fromData rawInlineDatum of
      Just (datum :: VoteDatum) -> (datum # unwrap # _.proposalTokenName) ==
        proposalTokenName
      _ -> false
    _ -> false

-- | Check for the presence of a token with the given symbol
-- | at the provided transactioun output
hasTokenWithSymbol ::
  CurrencySymbol ->
  (TransactionInput /\ TransactionOutputWithRefScript) ->
  Boolean
hasTokenWithSymbol symbol (_ /\ TransactionOutputWithRefScript txOut) =
  any (_ == symbol) $ symbols (txOut.output # unwrap # _.amount)

-- | Check for the presence of a token with the given symbol
-- | at the provided transactioun output
hasOneOfToken ::
  CurrencySymbol ->
  TokenName ->
  (TransactionInput /\ TransactionOutputWithRefScript) ->
  Boolean
hasOneOfToken symbol tokenName (_ /\ TransactionOutputWithRefScript txOut) =
  valueOf (txOut.output # unwrap # _.amount) symbol tokenName == one

-- | Result type for 'findKeyUtxoBySymbol' function
type SpendPubKeyResult =
  { lookups :: Lookups.ScriptLookups
  , constraints :: Constraints.TxConstraints
  , value :: Value
  }

-- | Get all the utxos that are owned by the wallet.
getAllWalletUtxos ::
  Contract (Map TransactionInput TransactionOutputWithRefScript)
getAllWalletUtxos = liftedM "Could not get users UTxOs" getWalletUtxos

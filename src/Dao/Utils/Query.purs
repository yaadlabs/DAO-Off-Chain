{-|
Module: Dao.Utils.Query
Description: Query helpers
-}
module Dao.Utils.Query
  ( UtxoInfo
  , QueryType(..)
  , SpendPubKeyResult
  , getAllWalletUtxos
  , findScriptUtxoBySymbol
  , findKeyUtxoBySymbol
  ) where

import Contract.Address (scriptHashAddress)
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
  , pure
  , show
  , (#)
  , ($)
  , (/\)
  , (<>)
  , (==)
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts (Validator, validatorHash)
import Contract.Transaction
  ( TransactionInput
  , TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  )
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value
  ( CurrencySymbol
  , Value
  , symbols
  )
import Contract.Wallet (getWalletUtxos)
import Data.Array (filter, head)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap)
import Type.Proxy (Proxy)

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

  let
    hasNft (_ /\ TransactionOutputWithRefScript txOut) =
      any (_ == symbol) $ symbols (txOut.output # unwrap # _.amount)

  (txIn /\ TransactionOutputWithRefScript txOut) ::
    (TransactionInput /\ TransactionOutputWithRefScript) <-
    liftContractM "Cannot find UTxO with NFT"
      $ head
      $ filter hasNft
      $ Map.toUnfoldable
      $ utxos

  let
    constraints' :: Constraints.TxConstraints
    constraints' =
      case spendOrReference of
        Spend -> Constraints.mustSpendScriptOutput txIn redeemer
        Reference -> Constraints.mustReferenceOutput txIn

    lookups' :: Lookups.ScriptLookups
    lookups' = mconcat
      [ Lookups.unspentOutputs $ Map.singleton txIn
          (TransactionOutputWithRefScript txOut)
      , Lookups.validator validatorScript
      ]

    value :: Value
    value = txOut.output # unwrap # _.amount

  case txOut.output # unwrap # _.datum of
    OutputDatum (Datum rawInlineDatum) -> case fromData rawInlineDatum of
      Just (datum :: datum') -> do
        pure
          { datum
          , value
          , lookups: lookups'
          , constraints: constraints'
          }
      Nothing -> throwContractError "Cannot parse datum"
    dat -> throwContractError $ "Missing inline datum, got: " <> show dat

-- | Result type for 'findKeyUtxoBySymbol' function
type SpendPubKeyResult =
  { lookups :: Lookups.ScriptLookups
  , constraints :: Constraints.TxConstraints
  , value :: Value
  }

-- | Spend UTXO marked by an NFT with the given CurrencySymbol
findKeyUtxoBySymbol ::
  CurrencySymbol ->
  Map TransactionInput TransactionOutputWithRefScript ->
  Contract SpendPubKeyResult
findKeyUtxoBySymbol symbol utxos = do
  logInfo' "Entering findKeyUtxoBySymbol contract"

  let
    hasNft (_ /\ TransactionOutputWithRefScript txOut) =
      any (_ == symbol) $ symbols (txOut.output # unwrap # _.amount)

  (txIn /\ TransactionOutputWithRefScript txOut) <-
    liftContractM "Cannot find UTxO with NFT"
      $ head
      $ filter hasNft
      $ Map.toUnfoldable
      $ utxos

  let
    lookups :: Lookups.ScriptLookups
    lookups = mconcat [ Lookups.unspentOutputs utxos ] -- TODO: Make this more precise

    constraints :: Constraints.TxConstraints
    constraints = mconcat [ Constraints.mustSpendPubKeyOutput txIn ]

    value :: Value
    value = txOut.output # unwrap # _.amount

  pure { lookups, constraints, value }

-- | Get all the utxos that are owned by the wallet.
getAllWalletUtxos ::
  Contract (Map TransactionInput TransactionOutputWithRefScript)
getAllWalletUtxos = liftedM "Could not get users UTxOs" getWalletUtxos

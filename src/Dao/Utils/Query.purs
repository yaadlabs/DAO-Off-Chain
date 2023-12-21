-- | Query helpers
module Dao.Utils.Query
  ( findUtxoByValue
  , getAllWalletUtxos
  ) where

import Contract.Prelude

import Contract.Monad (Contract, liftedM)
import Contract.Transaction
  ( TransactionInput
  , TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  )
import Contract.Utxos (UtxoMap)
import Contract.Value (Value)
import Contract.Wallet (getWalletUtxos)
import Data.Array as Array
import Data.Map (Map, mapMaybe, toUnfoldable)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap)

-- | Find the UTXO in the given 'UtxoMap'
-- | that holds the given 'Value'
findUtxoByValue ::
  Value ->
  UtxoMap ->
  Contract (Maybe (TransactionInput /\ TransactionOutputWithRefScript))
findUtxoByValue value = pure <<< getFirstTxInputInMap <<< findUtxoByValue' value

-- | Returns 'UtxoMap' containing only the entry that holds the given 'Value'
-- | and the 'unitDatum'. Will return empty map if the UTXO is not found.
findUtxoByValue' ::
  Value ->
  UtxoMap ->
  UtxoMap
findUtxoByValue' value utxoMap =
  mapMaybe op utxoMap
  where
  op ts@(TransactionOutputWithRefScript { output: txOut, scriptRef: _ })
    | (unwrap txOut).amount == value = Just ts
    | otherwise = Nothing

-- | Return the first UTXO in the given 'UtxoMap'
-- | as (TransactionInput, TransactionOutputWithRefScript) pair
-- | Returns 'Nothing' if the map is empty
getFirstTxInputInMap ::
  UtxoMap -> Maybe (TransactionInput /\ TransactionOutputWithRefScript)
getFirstTxInputInMap = Array.head <<< toUnfoldable

-- | Get all the utxos that are owned by the wallet.
getAllWalletUtxos ::
  Contract (Map TransactionInput TransactionOutputWithRefScript)
getAllWalletUtxos = liftedM "Could not get users UTxOs" getWalletUtxos

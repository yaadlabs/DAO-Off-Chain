module Dao.Utils
  ( getAllWalletUtxos
  , mkTokenName
  , getInlineDatum
  , getInlineDatumFromTxOutWithRefScript
  ) where

import Contract.Monad (Contract, liftedM)
import Contract.PlutusData (Datum)
import Contract.Prelude ((>>=))
import Contract.Prim.ByteArray (hexToByteArray)
import Contract.Transaction
  ( OutputDatum
      ( OutputDatum
      , OutputDatumHash
      , NoOutputDatum
      )
  , TransactionInput
  , TransactionOutput(TransactionOutput)
  , TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  )
import Contract.Value (TokenName, mkTokenName) as Value
import Contract.Wallet (getWalletUtxos)
import Data.Map (Map)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap)

-- | Get all the utxos that are owned by the wallet.
getAllWalletUtxos ::
  Contract (Map TransactionInput TransactionOutputWithRefScript)
getAllWalletUtxos = liftedM "Could not get users UTxOs" getWalletUtxos

mkTokenName :: String -> Maybe Value.TokenName
mkTokenName tn = hexToByteArray tn >>= Value.mkTokenName

getInlineDatum :: OutputDatum -> Maybe Datum
getInlineDatum outputDatum = case outputDatum of
  OutputDatum datum -> Just datum
  _ -> Nothing

getInlineDatumFromTxOutWithRefScript ::
  TransactionOutputWithRefScript -> Maybe Datum
getInlineDatumFromTxOutWithRefScript
  (TransactionOutputWithRefScript { output }) =
  case (unwrap output).datum of
    OutputDatum datum' -> Just datum'
    _ -> Nothing

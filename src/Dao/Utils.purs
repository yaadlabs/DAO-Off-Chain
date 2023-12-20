module Dao.Utils
  ( getAllWalletUtxos
  , mkTokenName
  ) where

import Contract.Monad (Contract, liftedM)
import Contract.Prelude ((>>=))
import Contract.Prim.ByteArray (hexToByteArray)
import Contract.Transaction (TransactionInput, TransactionOutputWithRefScript)
import Contract.Value (TokenName, mkTokenName) as Value
import Contract.Wallet (getWalletUtxos)
import Data.Map (Map)
import Data.Maybe (Maybe)

-- | Get all the utxos that are owned by the wallet.
getAllWalletUtxos ::
  Contract (Map TransactionInput TransactionOutputWithRefScript)
getAllWalletUtxos = liftedM "Could not get users UTxOs" getWalletUtxos

mkTokenName :: String -> Maybe Value.TokenName
mkTokenName tn = hexToByteArray tn >>= Value.mkTokenName

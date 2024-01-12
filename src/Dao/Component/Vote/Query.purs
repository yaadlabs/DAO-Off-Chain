module Dao.Component.Vote.Query (spendVoteNftUtxo) where

import Contract.Log (logInfo')
import Contract.Monad (Contract)
import Contract.Prelude (discard)
import Contract.Scripts (Validator)
import Contract.Transaction (TransactionInput, TransactionOutputWithRefScript)
import Contract.Value (CurrencySymbol)
import Contract.Value (Value)
import Dao.Utils.Query (findKeyUtxoBySymbol)
import Data.Map (Map)

spendVoteNftUtxo ::
  CurrencySymbol ->
  Map TransactionInput TransactionOutputWithRefScript ->
  Contract Value
spendVoteNftUtxo voteNftSymbol utxoMap = do
  logInfo' "Entering spendVoteNftUtxo contract"
  findKeyUtxoBySymbol voteNftSymbol utxoMap

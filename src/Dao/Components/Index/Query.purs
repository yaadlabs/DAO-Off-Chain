module Dao.Component.Index.Query (getIndexUtxo) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.Transaction (TransactionInput, TransactionOutputWithRefScript)
import Data.Typelevel.Undefined (undefined)

getIndexUtxo :: Contract (TransactionInput /\ TransactionOutputWithRefScript)
getIndexUtxo = undefined

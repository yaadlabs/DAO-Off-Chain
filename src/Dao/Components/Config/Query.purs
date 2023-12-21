module Dao.Component.Config.Query (getConfigUtxo) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.Transaction (TransactionInput, TransactionOutputWithRefScript)
import Data.Typelevel.Undefined (undefined)

getConfigUtxo :: Contract (TransactionInput /\ TransactionOutputWithRefScript)
getConfigUtxo = undefined

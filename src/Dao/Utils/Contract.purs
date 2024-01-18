module Dao.Utils.Contract (ContractResult(ContractResult)) where

import Contract.Transaction (TransactionHash)
import Contract.Value (CurrencySymbol, TokenName)

newtype ContractResult = ContractResult
  { txHash :: TransactionHash
  , symbol :: CurrencySymbol
  , tokenName :: TokenName
  }

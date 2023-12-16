module ScriptArguments.Types 
  ( NftConfig(..)
  , ConfigurationValidatorConfig(..) 
  , IndexNftConfig(..)
  ) where

import Contract.Value (CurrencySymbol, TokenName)
import Contract.Transaction (TransactionInput)
import Ctl.Internal.Serialization.Hash (ScriptHash)

newtype NftConfig = NftConfig
  { ncInitialUtxo :: TransactionInput
  , ncTokenName :: TokenName
  }

newtype ConfigurationValidatorConfig = ConfigurationValidatorConfig
  { cvcConfigNftCurrencySymbol :: CurrencySymbol
  , cvcConfigNftTokenName :: TokenName
  }

newtype IndexNftConfig = IndexNftConfig
  { incInitialUtxo :: TransactionInput
  , incTokenName :: TokenName
  , incIndexValidator :: ScriptHash
  }

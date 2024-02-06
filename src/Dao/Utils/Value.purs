{-|
Module: Dao.Utils.Value
Description: Value related helpers
-}
module Dao.Utils.Value
  ( mkTokenName
  , countOfTokenInValue
  ) where

import Contract.AssocMap as AssocMap
import Contract.Prelude (sum, zero, (<$>), (<=<))
import Contract.Prim.ByteArray (byteArrayFromAscii, hexToByteArray)
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , Value
  , getValue
  , mkTokenName
  ) as Value
import Data.Maybe (Maybe, fromMaybe)
import JS.BigInt (BigInt)

mkTokenName :: String -> Maybe Value.TokenName
mkTokenName = Value.mkTokenName <=< byteArrayFromAscii

countOfTokenInValue :: Value.CurrencySymbol -> Value.Value -> BigInt
countOfTokenInValue symbol value =
  let
    maybeTotal = sum <$> AssocMap.elems <$> AssocMap.lookup symbol
      (Value.getValue value)
  in
    fromMaybe zero maybeTotal

{-|
Module: Dao.Utils.Value
Description: Value related helpers
-}
module Dao.Utils.Value
  ( mkTokenName
  , valueSubtraction
  , normaliseValue
  , allPositive
  ) where

import Cardano.Types (AssetName, BigNum, ScriptHash, Value)
import Cardano.Types.AssetName (mkAssetName)
import Cardano.Types.BigNum (sub, zero) as BigNum
import Cardano.Types.Value (flatten, isPositive, unflatten, unionWith) as Value
import Contract.Prelude
  ( class Foldable
  , type (/\)
  , all
  , foldMap
  , sub
  , sum
  , zero
  , ($)
  , (/=)
  , (/\)
  , (<$>)
  , (<<<)
  , (<=<)
  , (>=)
  )
import Contract.Prim.ByteArray (byteArrayFromAscii, hexToByteArray)
import Data.Array (filter) as Array
import Data.Maybe (Maybe, fromJust, fromMaybe)
import Data.Tuple (Tuple(Tuple))
import JS.BigInt (BigInt)
import Partial.Unsafe (unsafePartial)

mkTokenName :: String -> Maybe AssetName
mkTokenName = mkAssetName <=< byteArrayFromAscii

allPositive :: Value -> Boolean
allPositive = Value.isPositive

valueSubtraction :: Value -> Value -> Maybe Value
valueSubtraction = Value.unionWith BigNum.sub

normaliseValue :: Value -> Value
normaliseValue =
  unsafePartial fromJust
    <<< Value.unflatten
    <<< Array.filter (\(Tuple _ amount) -> amount /= BigNum.zero)
    <<< Value.flatten

-- FIXME
{-
countOfTokenInValue :: ScriptHash -> Value -> BigNum
countOfTokenInValue symbol value
  let
    maybeTotal = sum <$> AssocMap.elems <$> AssocMap.lookup symbol
      (getValue value)
  in
    fromMaybe zero maybeTotal
-}

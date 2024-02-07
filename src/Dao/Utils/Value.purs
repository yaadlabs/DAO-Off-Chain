{-|
Module: Dao.Utils.Value
Description: Value related helpers
-}
module Dao.Utils.Value
  ( mkTokenName
  , valueSubtraction
  , normaliseValue
  , allPositive
  , countOfTokenInValue
  ) where

import Contract.AssocMap as AssocMap
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
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , Value
  , flattenValue
  , getValue
  , getValue
  , singleton
  , unionWith
  )
import Contract.Value
  ( mkTokenName
  ) as Value
import Data.Array (filter) as Array
import Data.Maybe (Maybe, fromMaybe)
import JS.BigInt (BigInt)

mkTokenName :: String -> Maybe TokenName
mkTokenName = Value.mkTokenName <=< byteArrayFromAscii

allPositive :: Value -> Boolean
allPositive = all (all (_ >= zero)) <<< getValue

valueSubtraction :: Value -> Value -> Value
valueSubtraction = unionWith sub

normaliseValue :: Value -> Value
normaliseValue = go $ Array.filter \(_ /\ _ /\ amount) -> amount /= zero
  where
  go op = unflattenValue <<< op <<< flattenValue

unflattenValue ::
  forall f.
  Foldable f =>
  f (CurrencySymbol /\ TokenName /\ BigInt) ->
  Value
unflattenValue = foldMap $
  \(symbol /\ tokenName /\ amount) -> singleton symbol tokenName amount

countOfTokenInValue :: CurrencySymbol -> Value -> BigInt
countOfTokenInValue symbol value =
  let
    maybeTotal = sum <$> AssocMap.elems <$> AssocMap.lookup symbol
      (getValue value)
  in
    fromMaybe zero maybeTotal

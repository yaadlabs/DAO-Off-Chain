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

import Contract.Prelude
  ( class Foldable
  , type (/\)
  , all
  , foldMap
  , sub
  , zero
  , ($)
  , (/=)
  , (/\)
  , (<<<)
  , (<=<)
  , (>=)
  )
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , Value
  , flattenValue
  , getValue
  , singleton
  , unionWith
  )
import Contract.Value
  ( mkTokenName
  ) as Value
import Data.Array (filter) as Array
import Data.Maybe (Maybe)
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

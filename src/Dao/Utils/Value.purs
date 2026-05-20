{-|
Module: Dao.Utils.Value
Description: Value related helpers
-}
module Dao.Utils.Value
  ( countOfTokenInValue
  , mkTokenName
  , valueSubtraction
  , normaliseValue
  , allPositive
  ) where

import Cardano.Types (AssetName, BigNum, ScriptHash, Value)
import Cardano.Types.AssetName (mkAssetName)
import Cardano.Types.BigNum (add, sub, zero) as BigNum
import Cardano.Types.Value
  ( flatten
  , getMultiAsset
  , isPositive
  , unflatten
  , unionWith
  ) as Value
import Contract.Prelude (($), (/=), (<<<), (<=<), (=<<))
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Data.Array (filter) as Array
import Data.Foldable (foldl)
import Data.Map (lookup, values) as Map
import Data.Maybe (Maybe(Just), fromJust, fromMaybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(Tuple))
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

countOfTokenInValue :: ScriptHash -> Value -> BigNum
countOfTokenInValue symbol val =
  fromMaybe BigNum.zero
    ( (foldl (\acc x -> BigNum.add x =<< acc) (Just BigNum.zero) <<< Map.values)
        =<< Map.lookup symbol (unwrap $ Value.getMultiAsset val)
    )

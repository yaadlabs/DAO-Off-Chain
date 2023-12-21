{-|
Module: Dao.Utils.Value
Description: Value related helpers
-}
module Dao.Utils.Value
  ( mkTokenName
  ) where

import Contract.Prelude ((>>=))
import Contract.Prim.ByteArray (hexToByteArray)
import Contract.Value (TokenName, mkTokenName) as Value
import Data.Maybe (Maybe)

mkTokenName :: String -> Maybe Value.TokenName
mkTokenName tn = hexToByteArray tn >>= Value.mkTokenName

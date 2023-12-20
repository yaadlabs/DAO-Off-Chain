module Scripts.Utils where

import Contract.Prelude
import LambdaBuffers.Runtime.Prelude

import Contract.Prim.ByteArray (ByteArray(ByteArray))
import Data.Newtype (unwrap)
import Data.TextEncoder (encodeUtf8)
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)

toJsonBytes' :: forall a. Json a => a -> ByteArray
toJsonBytes' = ByteArray <<< encodeUtf8 <<< toJsonString

scriptStringToJson :: Json String => String -> String
scriptStringToJson scriptString =
  either
    (\_ -> error' "Error config validator: fromJsonString")
    identity
    (fromJsonString scriptString)

error' :: forall a. String -> a
error' = unsafePerformEffect <<< throw

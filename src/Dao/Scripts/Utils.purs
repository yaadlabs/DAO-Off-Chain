module Dao.Scripts.Utils
  ( mkUnappliedPolicy
  , mkUnappliedValidator
  , mkScript
  ) where

import Contract.Prelude

import Contract.Monad (Contract, liftContractE)
import Contract.PlutusData (class ToData, toData)
import Contract.Prim.ByteArray (ByteArray(ByteArray))
import Contract.Scripts
  ( MintingPolicy(PlutusMintingPolicy)
  , PlutusScript
  , Validator(Validator)
  , applyArgs
  )
import Ctl.Internal.Types.Scripts (plutusV2Script)
import Data.Newtype (unwrap)
import Data.TextEncoder (encodeUtf8)
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)
import LambdaBuffers.Runtime.Prelude
  ( class Json
  , Bytes(Bytes)
  , fromJsonString
  , toJsonString
  )
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync as NodeFS

mkUnappliedPolicy ::
  forall param. ToData param => String -> param -> Contract MintingPolicy
mkUnappliedPolicy filePath param = do
  appliedPolicy <- liftContractE $ mkScript filePath `applyArgs`
    [ toData param ]
  pure $ PlutusMintingPolicy appliedPolicy

mkUnappliedValidator ::
  forall param. ToData param => String -> param -> Contract Validator
mkUnappliedValidator filePath param = do
  appliedValidator <- liftContractE $ mkScript filePath `applyArgs`
    [ toData param ]
  pure $ Validator appliedValidator

mkScript :: String -> PlutusScript
mkScript = plutusV2Script <<< lbBytesToByteArray <<< scriptToBytesFromFile

scriptToBytesFromFile :: String -> Bytes
scriptToBytesFromFile = scriptStringToBytes <<< unsafeReadFile

lbBytesToByteArray :: Bytes -> ByteArray
lbBytesToByteArray (Bytes uint8Array) = ByteArray uint8Array

scriptStringToBytes :: Json String => String -> Bytes
scriptStringToBytes scriptString =
  either
    (\_ -> error' $ "Error config validator: fromJsonString: " <> scriptString)
    identity
    (fromJsonString scriptString)

unsafeReadFile :: String -> String
unsafeReadFile = unsafePerformEffect <<< NodeFS.readTextFile UTF8

error' :: forall a. String -> a
error' = unsafePerformEffect <<< throw

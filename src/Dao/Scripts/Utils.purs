module Dao.Scripts.Utils
  ( mkUnappliedPolicy
  , mkUnappliedPolicy'
  , mkUnappliedValidator
  , mkUnappliedValidator'
  , mkScript
  , mkScript'
  ) where

import Contract.Prelude

import Cardano.Plutus.ApplyArgs (applyArgs)
import Cardano.Types (PlutusScript, RawBytes)
import Cardano.Types.PlutusScript (plutusV2Script)
import Contract.Monad (Contract, liftContractE)
import Contract.PlutusData (class ToData, toData)
import Contract.Prim.ByteArray (ByteArray(ByteArray))
import Data.Newtype (wrap)
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)
import LambdaBuffers.Runtime.Prelude
  ( class Json
  , Bytes(Bytes)
  , fromJsonString
  )
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync as NodeFS

mkUnappliedPolicy ::
  forall param. ToData param => String -> param -> Contract PlutusScript
mkUnappliedPolicy filePath param = do
  appliedPolicy <- liftContractE $ mkScript filePath `applyArgs`
    [ toData param ]
  pure appliedPolicy

mkUnappliedPolicy' ::
  forall param. ToData param => String -> param -> Contract PlutusScript
mkUnappliedPolicy' scriptString param = do
  appliedPolicy <- liftContractE $ mkScript' scriptString `applyArgs`
    [ toData param ]
  pure appliedPolicy

mkUnappliedValidator ::
  forall param. ToData param => String -> param -> Contract PlutusScript
mkUnappliedValidator filePath param = do
  appliedValidator <- liftContractE $ mkScript filePath `applyArgs`
    [ toData param ]
  pure appliedValidator

mkUnappliedValidator' ::
  forall param. ToData param => String -> param -> Contract PlutusScript
mkUnappliedValidator' scriptString param = do
  appliedValidator <- liftContractE $ mkScript' scriptString `applyArgs`
    [ toData param ]
  pure appliedValidator

-- | Makes a PlutusScript from a JSON file containing a single string
mkScript :: String -> PlutusScript
mkScript = plutusV2Script <<< lbBytesToByteArray <<< scriptToBytesFromFile

-- | Makes a PlutusScript from a string
mkScript' :: String -> PlutusScript
mkScript' = plutusV2Script <<< lbBytesToByteArray <<< scriptStringToBytes'

scriptToBytesFromFile :: String -> Bytes
scriptToBytesFromFile = scriptStringToBytes <<< unsafeReadFile

lbBytesToByteArray :: Bytes -> RawBytes
lbBytesToByteArray (Bytes uint8Array) = wrap $ ByteArray uint8Array

scriptStringToBytes :: Json String => String -> Bytes
scriptStringToBytes scriptString =
  either
    (\_ -> error' $ "Error config validator: fromJsonString: " <> scriptString)
    identity
    (fromJsonString scriptString)

scriptStringToBytes' :: String -> Bytes
scriptStringToBytes' scriptString =
  either
    (\_ -> error' $ "Error config validator: fromJsonString: " <> scriptString)
    identity
    (fromJsonString $ "\"" <> scriptString <> "\"")

unsafeReadFile :: String -> String
unsafeReadFile = unsafePerformEffect <<< NodeFS.readTextFile UTF8

error' :: forall a. String -> a
error' = unsafePerformEffect <<< throw

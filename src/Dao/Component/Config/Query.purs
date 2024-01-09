module Dao.Component.Config.Query (ConfigInfo, getConfigInfo) where

import Contract.Prelude

import Contract.Address (scriptHashAddress)
import Contract.Log (logInfo')
import Contract.Monad
  ( Contract
  , liftContractM
  , throwContractError
  )
import Contract.PlutusData
  ( Datum(Datum)
  , OutputDatum(OutputDatum)
  , fromData
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts (Validator, validatorHash)
import Contract.Transaction
  ( TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  )
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value
  ( CurrencySymbol
  , Value
  , symbols
  )
import Data.Array (filter, head)
import Data.Map as Map
import LambdaBuffers.ApplicationTypes.Configuration (DynamicConfigDatum)

type ConfigInfo =
  { lookups :: Lookups.ScriptLookups
  , constraints :: Constraints.TxConstraints
  , configDatum :: DynamicConfigDatum
  , configValue :: Value
  }

getConfigInfo ::
  CurrencySymbol ->
  Validator ->
  Contract ConfigInfo
getConfigInfo configSymbol configValidator = do
  logInfo' "Entering getConfigInfo contract"

  let
    scriptAddr = scriptHashAddress (validatorHash configValidator) Nothing

  utxos <- utxosAt scriptAddr

  let
    hasNft (_ /\ TransactionOutputWithRefScript txOut) =
      any (_ == configSymbol) $ symbols (txOut.output # unwrap # _.amount)

  (txIn /\ TransactionOutputWithRefScript configUtxo) <-
    liftContractM "Cannot find UTxO with NFT"
      $ head
      $ filter hasNft
      $ Map.toUnfoldable
      $ utxos

  let
    constraints' :: Constraints.TxConstraints
    constraints' = Constraints.mustReferenceOutput txIn

    lookups' :: Lookups.ScriptLookups
    lookups' = Lookups.unspentOutputs $
      Map.singleton txIn (TransactionOutputWithRefScript configUtxo)

    configValue :: Value
    configValue = configUtxo.output # unwrap # _.amount

  case configUtxo.output # unwrap # _.datum of
    OutputDatum (Datum rawInlineDatum) -> case fromData rawInlineDatum of
      Just (configDatum :: DynamicConfigDatum) -> do
        pure
          { configDatum
          , configValue
          , lookups: lookups'
          , constraints: constraints'
          }
      Nothing -> throwContractError "Cannot parse config datum"
    dat -> throwContractError $ "Missing inline datum, got: " <> show dat

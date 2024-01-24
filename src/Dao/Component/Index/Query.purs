module Dao.Component.Index.Query (IndexInfo, getIndexInfo) where

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
  , unitRedeemer
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts (validatorHash)
import Contract.Transaction
  ( TransactionInput
  , TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  , TransactionUnspentOutput(TransactionUnspentOutput)
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
import LambdaBuffers.ApplicationTypes.Index (IndexNftDatum)
import Scripts.IndexValidator (indexValidatorScript, indexValidatorScriptDebug)

type IndexInfo =
  { lookups :: Lookups.ScriptLookups
  , constraints :: Constraints.TxConstraints
  , indexDatum :: IndexNftDatum
  , indexValue :: Value
  }

getIndexInfo ::
  CurrencySymbol ->
  Contract IndexInfo
getIndexInfo indexSymbol = do
  logInfo' "Entering getIndexInfo contract"

  indexValidator <- indexValidatorScriptDebug

  let
    scriptAddr = scriptHashAddress (validatorHash indexValidator) Nothing

  utxos <- utxosAt scriptAddr

  let
    hasNft (_ /\ TransactionOutputWithRefScript txOut) =
      any (_ == indexSymbol) $ symbols (txOut.output # unwrap # _.amount)

  (txIn /\ txOut@(TransactionOutputWithRefScript indexUtxo)) <-
    liftContractM "Cannot find UTxO with NFT"
      $ head
      $ filter hasNft
      $ Map.toUnfoldable
      $ utxos

  let
    constraints' :: Constraints.TxConstraints
    constraints' = Constraints.mustSpendScriptOutput txIn unitRedeemer

    lookups' :: Lookups.ScriptLookups
    lookups' = Lookups.unspentOutputs $ Map.singleton txIn txOut

    indexValue :: Value
    indexValue = indexUtxo.output # unwrap # _.amount

  case indexUtxo.output # unwrap # _.datum of
    OutputDatum (Datum rawInlineDatum) -> case fromData rawInlineDatum of
      Just (indexDatum :: IndexNftDatum) -> do
        pure
          { indexDatum
          , indexValue
          , lookups: lookups'
          , constraints: constraints'
          }
      Nothing -> throwContractError "Cannot parse index datum"
    dat -> throwContractError $ "Missing inline datum, got: " <> show dat

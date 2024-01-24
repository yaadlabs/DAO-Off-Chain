{-|
Module: Dao.Workflow.CreateIndex
Description: Contract for creating index datum
  and locking it at UTXO at index validator marked by index NFT
-}
module Dao.Workflow.CreateIndex (createIndex) where

import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData (Datum(Datum), toData)
import Contract.Prelude
  ( type (/\)
  , bind
  , discard
  , mconcat
  , one
  , pure
  , ($)
  , (/\)
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy, Validator, ValidatorHash, validatorHash)
import Contract.Transaction
  ( TransactionHash
  , TransactionInput
  , TransactionOutputWithRefScript
  , submitTxFromConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , Value
  , scriptCurrencySymbol
  )
import Contract.Value (singleton) as Value
import Dao.Utils.Query (getAllWalletUtxos)
import Data.Array (head)
import Data.Map as Map
import Data.Newtype (unwrap)
import JS.BigInt (fromInt)
import LambdaBuffers.ApplicationTypes.Index (IndexNftDatum(IndexNftDatum))
import ScriptArguments.Types (IndexNftConfig(IndexNftConfig))
import Scripts.IndexPolicy (unappliedIndexPolicy, unappliedIndexPolicyDebug)
import Scripts.IndexValidator (indexValidatorScript, indexValidatorScriptDebug)

-- | Contract for creating index datum and locking 
-- it at UTXO at index validator marked by index NFT
createIndex ::
  TokenName -> Contract (TransactionHash /\ CurrencySymbol /\ TokenName)
createIndex indexTokenName = do
  logInfo' "Entering createIndex transaction"

  userUtxos <- getAllWalletUtxos

  configSpend <- liftContractM "No UTXOs found"
    $ head
    $ Map.toUnfoldable userUtxos

  indexInfo <- buildIndex configSpend indexTokenName

  let
    lookups :: Lookups.ScriptLookups
    lookups = indexInfo.lookups

    constraints :: Constraints.TxConstraints
    constraints = indexInfo.constraints

  txHash <- submitTxFromConstraints lookups constraints

  pure (txHash /\ indexInfo.symbol /\ indexTokenName)

type IndexInfo =
  { symbol :: CurrencySymbol
  , lookups :: Lookups.ScriptLookups
  , constraints :: Constraints.TxConstraints
  }

buildIndex ::
  (TransactionInput /\ TransactionOutputWithRefScript) ->
  TokenName ->
  Contract IndexInfo
buildIndex (txInput /\ txInputWithScript) indexTokenName =
  do
    logInfo' "Entering buildIndex transaction"

    indexValidator :: Validator <- indexValidatorScriptDebug

    let
      indexValidatorHash :: ValidatorHash
      indexValidatorHash = validatorHash indexValidator

      indexPolicyParams :: IndexNftConfig
      indexPolicyParams = IndexNftConfig
        { incInitialUtxo: txInput
        , incTokenName: indexTokenName
        , incIndexValidator: unwrap indexValidatorHash
        }

    appliedIndexPolicy :: MintingPolicy <- unappliedIndexPolicyDebug
      indexPolicyParams

    let
      indexSymbol :: CurrencySymbol
      indexSymbol = scriptCurrencySymbol appliedIndexPolicy

      indexNft :: Value
      indexNft = Value.singleton indexSymbol indexTokenName one

      indexDatum' :: IndexNftDatum
      indexDatum' = IndexNftDatum { index: fromInt 0 }

      indexDatum :: Datum
      indexDatum = Datum $ toData indexDatum'

      lookups' :: Lookups.ScriptLookups
      lookups' = mconcat
        [ Lookups.mintingPolicy appliedIndexPolicy
        , Lookups.unspentOutputs $ Map.singleton txInput txInputWithScript
        ]

      constraints' :: Constraints.TxConstraints
      constraints' = mconcat
        [ Constraints.mustMintValue indexNft
        , Constraints.mustSpendPubKeyOutput txInput
        , Constraints.mustPayToScript
            indexValidatorHash
            indexDatum
            Constraints.DatumInline
            indexNft
        ]

    pure { symbol: indexSymbol, lookups: lookups', constraints: constraints' }

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
import Dao.Scripts.Policy.Index (unappliedIndexPolicyDebug)
import Dao.Scripts.Validator.Index (indexValidatorScriptDebug)
import Dao.Utils.Contract (ContractResult(ContractResult))
import Dao.Utils.Query (getAllWalletUtxos)
import Data.Array (head)
import Data.Map as Map
import Data.Newtype (unwrap)
import JS.BigInt (fromInt)
import LambdaBuffers.ApplicationTypes.Index (IndexDatum(IndexDatum))
import ScriptArguments.Types (IndexPolicyParams(IndexPolicyParams))

-- | Contract for creating index datum and locking 
-- it at UTXO at index validator marked by index NFT
createIndex ::
  TokenName -> Contract ContractResult
createIndex tokenName = do
  logInfo' "Entering createIndex transaction"

  userUtxos <- getAllWalletUtxos

  configSpend <- liftContractM "No UTXOs found"
    $ head
    $ Map.toUnfoldable userUtxos

  indexInfo <- buildIndex configSpend tokenName

  let
    lookups :: Lookups.ScriptLookups
    lookups = indexInfo.lookups

    constraints :: Constraints.TxConstraints
    constraints = indexInfo.constraints

    symbol :: CurrencySymbol
    symbol = indexInfo.symbol

  txHash <- submitTxFromConstraints lookups constraints

  pure $ ContractResult { txHash, symbol, tokenName }

type IndexInfo =
  { symbol :: CurrencySymbol
  , lookups :: Lookups.ScriptLookups
  , constraints :: Constraints.TxConstraints
  }

-- | Build the constraints and lookups for the tx
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

      indexPolicyParams :: IndexPolicyParams
      indexPolicyParams = IndexPolicyParams
        { ipInitialUtxo: txInput
        , ipTokenName: indexTokenName
        , ipIndexValidator: unwrap indexValidatorHash
        }

    appliedIndexPolicy :: MintingPolicy <- unappliedIndexPolicyDebug
      indexPolicyParams

    let
      indexSymbol :: CurrencySymbol
      indexSymbol = scriptCurrencySymbol appliedIndexPolicy

      -- The token that will mark the UTXO containing
      -- the index datum at the index validator
      indexNft :: Value
      indexNft = Value.singleton indexSymbol indexTokenName one

      -- The 'index' field of the datum keeps track of the number of proposals
      -- Hence, we need to set this to zero to initially
      indexDatum' :: IndexDatum
      indexDatum' = IndexDatum { index: fromInt 0 }

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
        -- ^ We pay the newly created index datum to a UTXO at
        -- the index validator, marked by the 'indexNft'
        ]

    pure { symbol: indexSymbol, lookups: lookups', constraints: constraints' }

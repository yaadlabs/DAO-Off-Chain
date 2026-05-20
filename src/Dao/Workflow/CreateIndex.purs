{-|
Module: Dao.Workflow.CreateIndex
Description: Contract for creating index datum
  and locking it at UTXO at index validator marked by index NFT
-}
module Dao.Workflow.CreateIndex (createIndex) where

import Cardano.ToData (toData)
import Cardano.Types
  ( AssetName
  , PlutusData
  , PlutusScript
  , ScriptHash
  , TransactionOutput
  , Value
  )
import Cardano.Types.BigNum (one) as BigNum
import Cardano.Types.Mint (fromMultiAsset) as Mint
import Cardano.Types.PlutusScript (hash) as PlutusScript
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM)
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
import Contract.Transaction
  ( TransactionHash
  , TransactionInput
  , submitTxFromConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Value (getMultiAsset, singleton) as Value
import Dao.Scripts.Policy (unappliedIndexPolicy)
import Dao.Scripts.Validator (indexValidatorScript)
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
createIndex :: AssetName -> Contract ContractResult
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

    symbol :: ScriptHash
    symbol = indexInfo.symbol

  txHash <- submitTxFromConstraints lookups constraints

  pure $ ContractResult { txHash, symbol, tokenName }

type IndexInfo =
  { symbol :: ScriptHash
  , lookups :: Lookups.ScriptLookups
  , constraints :: Constraints.TxConstraints
  }

-- | Build the constraints and lookups for the tx
buildIndex ::
  (TransactionInput /\ TransactionOutput) ->
  AssetName ->
  Contract IndexInfo
buildIndex (txInput /\ txInputWithScript) indexTokenName =
  do
    logInfo' "Entering buildIndex transaction"

    indexValidator :: PlutusScript <- indexValidatorScript

    let
      indexValidatorHash :: ScriptHash
      indexValidatorHash = PlutusScript.hash indexValidator

      indexPolicyParams :: IndexPolicyParams
      indexPolicyParams = IndexPolicyParams
        { ipInitialUtxo: txInput
        , ipTokenName: indexTokenName
        , ipIndexValidator: indexValidatorHash
        }

    appliedIndexPolicy :: PlutusScript <- unappliedIndexPolicy
      indexPolicyParams

    let
      indexSymbol :: ScriptHash
      indexSymbol = PlutusScript.hash appliedIndexPolicy

      -- The token that will mark the UTXO containing
      -- the index datum at the index validator
      indexNft :: Value
      indexNft = Value.singleton indexSymbol indexTokenName BigNum.one

      -- The 'index' field of the datum keeps track of the number of proposals
      -- Hence, we need to set this to zero to initially
      indexDatum' :: IndexDatum
      indexDatum' = IndexDatum { index: fromInt 0 }

      indexDatum :: PlutusData
      indexDatum = toData indexDatum'

      lookups' :: Lookups.ScriptLookups
      lookups' = mconcat
        [ Lookups.plutusMintingPolicy appliedIndexPolicy
        , Lookups.unspentOutputs $ Map.singleton txInput txInputWithScript
        ]

      constraints' :: Constraints.TxConstraints
      constraints' = mconcat
        [ Constraints.mustMintValue $ Mint.fromMultiAsset $ Value.getMultiAsset
            indexNft
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

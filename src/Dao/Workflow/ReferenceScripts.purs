module Dao.Workflow.ReferenceScripts
  ( deployReferenceScriptsOne
  , deployReferenceScriptsTwo
  , deployReferenceScriptsThree
  , retrieveReferenceScript
  ) where

import Contract.Address (scriptHashAddress)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData (unitDatum)
import Contract.Prelude
  ( Maybe(Just, Nothing)
  , Unit
  , bind
  , discard
  , foldMap
  , mconcat
  , mempty
  , pure
  , show
  , unwrap
  , ($)
  , (/\)
  , (==)
  )
import Contract.Scripts
  ( MintingPolicy(PlutusMintingPolicy, NativeMintingPolicy)
  , PlutusScript
  , Validator
  , validatorHash
  )
import Contract.Transaction
  ( ScriptRef(PlutusScriptRef)
  , TransactionHash
  , awaitTxConfirmedWithTimeout
  , mkTxUnspentOut
  , submitTxFromConstraints
  )
import Contract.TxConstraints
  ( DatumPresence(DatumInline)
  , InputWithScriptRef(RefInput)
  )
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Dao.Scripts.Policy (unappliedVotePolicy)
import Dao.Scripts.Validator
  ( unappliedConfigValidator
  , indexValidatorScript
  , unappliedTallyValidator
  , unappliedTreasuryValidator
  , unappliedVoteValidator
  )
import Data.Array (head, mapMaybe)
import Data.Map as Map
import Data.Time.Duration (Seconds(Seconds))
import ScriptArguments.Types (ValidatorParams)

-- | TODO: Reduce duplication
deployReferenceValidator' ::
  Contract Validator ->
  Contract Constraints.TxConstraints
deployReferenceValidator' validator' = do
  indexValidator <- indexValidatorScript
  validator <- validator'
  let referenceScript = PlutusScriptRef $ unwrap validator
  pure $
    mconcat
      [ Constraints.mustPayToScriptWithScriptRef
          (validatorHash indexValidator)
          unitDatum
          DatumInline
          referenceScript
          mempty
      ]

deployReferenceValidator ::
  ValidatorParams ->
  (ValidatorParams -> Contract Validator) ->
  Contract Constraints.TxConstraints
deployReferenceValidator validatorParams validator' = do
  indexValidator <- indexValidatorScript
  validator <- validator' validatorParams
  let referenceScript = PlutusScriptRef $ unwrap validator
  pure $
    mconcat
      [ Constraints.mustPayToScriptWithScriptRef
          (validatorHash indexValidator)
          unitDatum
          DatumInline
          referenceScript
          mempty
      ]

deployReferencePolicy ::
  ValidatorParams ->
  (ValidatorParams -> Contract MintingPolicy) ->
  Contract Constraints.TxConstraints
deployReferencePolicy validatorParams policy' = do
  indexValidator <- indexValidatorScript
  policy <- policy' validatorParams
  case policy of
    NativeMintingPolicy _ -> mempty
    PlutusMintingPolicy script -> do
      let referenceScript = PlutusScriptRef script
      pure $
        mconcat
          [ Constraints.mustPayToScriptWithScriptRef
              (validatorHash indexValidator)
              unitDatum
              DatumInline
              referenceScript
              mempty
          ]

deployReferenceScriptsOne :: ValidatorParams -> Contract TransactionHash
deployReferenceScriptsOne validatorParams = do
  logInfo' "Entering deployReferenceScripts"

  voteValidatorConstraints <- deployReferenceValidator validatorParams
    unappliedVoteValidator
  tallyValidatorConstraints <- deployReferenceValidator validatorParams
    unappliedTallyValidator

  let
    allConstraints = mconcat
      [ voteValidatorConstraints
      , tallyValidatorConstraints
      ]

  txId <- submitTxFromConstraints mempty allConstraints
  logInfo' $ mconcat [ "deployReferenceScripts tx submitted: ", show txId ]
  awaitTxConfirmedWithTimeout (Seconds 600.0) txId
  pure txId

deployReferenceScriptsTwo :: ValidatorParams -> Contract TransactionHash
deployReferenceScriptsTwo validatorParams = do
  logInfo' "Entering deployReferenceScripts"

  configValidatorConstraints <- deployReferenceValidator validatorParams
    unappliedConfigValidator
  treasuryValidatorConstraints <- deployReferenceValidator validatorParams
    unappliedTreasuryValidator

  let
    allConstraints = mconcat
      [ treasuryValidatorConstraints
      , configValidatorConstraints
      ]

  txId <- submitTxFromConstraints mempty allConstraints
  logInfo' $ mconcat [ "deployReferenceScripts tx submitted: ", show txId ]
  awaitTxConfirmedWithTimeout (Seconds 600.0) txId
  pure txId

deployReferenceScriptsThree :: ValidatorParams -> Contract TransactionHash
deployReferenceScriptsThree validatorParams = do
  logInfo' "Entering deployReferenceScripts"

  indexValidatorConstraints <- deployReferenceValidator'
    indexValidatorScript
  votePolicyConstraints <- deployReferencePolicy validatorParams
    unappliedVotePolicy

  let
    allConstraints = mconcat
      [ indexValidatorConstraints
      , votePolicyConstraints
      ]

  txId <- submitTxFromConstraints mempty allConstraints
  logInfo' $ mconcat [ "deployReferenceScripts tx submitted: ", show txId ]
  awaitTxConfirmedWithTimeout (Seconds 600.0) txId
  pure txId

retrieveReferenceScript ::
  PlutusScript ->
  Contract InputWithScriptRef
retrieveReferenceScript script = do
  indexValidator <- indexValidatorScript
  let
    scriptHolderAddress = scriptHashAddress
      (validatorHash indexValidator)
      Nothing
  utxos <- utxosAt scriptHolderAddress
  let
    findUtxoWithScript (txInp /\ txOut) =
      case (unwrap txOut).scriptRef of
        Just (PlutusScriptRef ref) ->
          if ref == script then Just $ RefInput $ mkTxUnspentOut txInp txOut
          else Nothing
        _ -> Nothing
    utxosList = Map.toUnfoldableUnordered utxos
  liftContractM "Could not find reference script"
    $ head
    $ mapMaybe
        findUtxoWithScript
        utxosList

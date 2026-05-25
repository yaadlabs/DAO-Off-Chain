module Dao.Workflow.ReferenceScripts
  ( deployReferenceScriptsOne
  , deployReferenceScriptsTwo
  , deployReferenceScriptsThree
  , retrieveReferenceScript
  ) where

import Cardano.Types (Credential(ScriptHashCredential), PlutusScript)
import Cardano.Types.Address (mkPaymentAddress)
import Cardano.Types.PlutusScript (hash) as PlutusScript
import Cardano.Types.Value (empty) as Value
import Contract.Address (getNetworkId)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData (unitDatum)
import Contract.Prelude
  ( Maybe(Just, Nothing)
  , bind
  , discard
  , mconcat
  , mempty
  , pure
  , show
  , unwrap
  , ($)
  , (/\)
  , (==)
  )
import Contract.Scripts (validatorHash)
import Contract.Transaction
  ( ScriptRef(PlutusScriptRef)
  , TransactionHash
  , awaitTxConfirmedWithTimeout
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
  ( indexValidatorScript
  , unappliedConfigValidator
  , unappliedTallyValidator
  , unappliedTreasuryValidator
  , unappliedVoteValidator
  )
import Data.Array (head, mapMaybe)
import Data.Map as Map
import Data.Newtype (wrap)
import Data.Time.Duration (Seconds(Seconds))
import ScriptArguments.Types (ValidatorParams)

-- | TODO: Reduce duplication
deployReferenceValidator' ::
  Contract PlutusScript ->
  Contract Constraints.TxConstraints
deployReferenceValidator' validator' = do
  indexValidator <- indexValidatorScript
  validator <- validator'
  let referenceScript = PlutusScriptRef validator
  pure $
    mconcat
      [ Constraints.mustPayToScriptWithScriptRef
          (validatorHash indexValidator)
          unitDatum
          DatumInline
          referenceScript
          Value.empty
      ]

deployReferenceValidator ::
  ValidatorParams ->
  (ValidatorParams -> Contract PlutusScript) ->
  Contract Constraints.TxConstraints
deployReferenceValidator validatorParams validator' = do
  indexValidator <- indexValidatorScript
  validator <- validator' validatorParams
  let referenceScript = PlutusScriptRef validator
  pure $
    mconcat
      [ Constraints.mustPayToScriptWithScriptRef
          (validatorHash indexValidator)
          unitDatum
          DatumInline
          referenceScript
          Value.empty
      ]

deployReferencePolicy ::
  ValidatorParams ->
  (ValidatorParams -> Contract PlutusScript) ->
  Contract Constraints.TxConstraints
deployReferencePolicy validatorParams policy' = do
  indexValidator <- indexValidatorScript
  script <- policy' validatorParams
  let referenceScript = PlutusScriptRef script
  pure $
    mconcat
      [ Constraints.mustPayToScriptWithScriptRef
          (validatorHash indexValidator)
          unitDatum
          DatumInline
          referenceScript
          Value.empty
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
  network <- getNetworkId
  let
    scriptHolderAddress =
      mkPaymentAddress
        network
        (wrap $ ScriptHashCredential $ PlutusScript.hash indexValidator)
        Nothing
  utxos <- utxosAt scriptHolderAddress
  let
    findUtxoWithScript (input /\ output) =
      case (unwrap output).scriptRef of
        Just (PlutusScriptRef ref) ->
          if ref == script then Just $ RefInput $ wrap { input, output }
          else Nothing
        _ -> Nothing
    utxosList = Map.toUnfoldableUnordered utxos
  liftContractM "Could not find reference script"
    $ head
    $ mapMaybe
        findUtxoWithScript
        utxosList

module Dao.Workflow.CreateProposal (createProposal) where

import Contract.Prelude

import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData (Datum(Datum), fromData, toData, unitRedeemer)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy, Validator, ValidatorHash, validatorHash)
import Contract.Transaction
  ( TransactionHash
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
import Dao.Component.Config.Query (getConfigUtxo)
import Dao.Component.Index.Query (getIndexUtxo)
import Dao.Utils
  ( getInlineDatumFromTxOutWithRefScript
  , mkTokenName
  )
import Data.Map as Map
import JS.BigInt (fromInt)
import LambdaBuffers.ApplicationTypes.Index (IndexNftDatum(IndexNftDatum))
import LambdaBuffers.ApplicationTypes.Tally (TallyStateDatum)
import ScriptArguments.Types
  ( ConfigurationValidatorConfig(ConfigurationValidatorConfig)
  , TallyNftConfig(TallyNftConfig)
  )
import Scripts.IndexValidator (indexValidatorScript)
import Scripts.TallyPolicy (unappliedTallyPolicy)
import Scripts.TallyValidator (unappliedTallyValidator)

createProposal ::
  TallyNftConfig ->
  TallyStateDatum ->
  Contract (TransactionHash /\ CurrencySymbol)
createProposal tallyConfig tallyStateDatum = do
  logInfo' "Entering createProposal transaction"

  (configUtxoTxInput /\ _) <- getConfigUtxo
  (indexUtxoTxInput /\ indexUtxoTxOutRefScript) <- getIndexUtxo

  oldIndexDatum' :: Datum <-
    liftContractM "No Inline index datum at OutputDatum" $
      getInlineDatumFromTxOutWithRefScript indexUtxoTxOutRefScript
  oldIndexDatum :: IndexNftDatum <-
    liftContractM "Could not convert datum" $ fromData (unwrap oldIndexDatum')

  appliedTallyPolicy :: MintingPolicy <- unappliedTallyPolicy tallyConfig

  let validatorConfig = mkValidatorConfig tallyConfig
  appliedTallyValidator :: Validator <- unappliedTallyValidator validatorConfig

  let
    updatedIndexDatum :: IndexNftDatum
    updatedIndexDatum = incrementIndexDatum oldIndexDatum

  tallyTokenName :: TokenName <- liftContractM "Could not make tally token name"
    $
      mkTallyTokenName updatedIndexDatum

  let
    tallySymbol :: CurrencySymbol
    tallySymbol = scriptCurrencySymbol appliedTallyPolicy

    indexValidatorHash :: ValidatorHash
    indexValidatorHash = validatorHash indexValidatorScript

    tallyValidatorHash :: ValidatorHash
    tallyValidatorHash = validatorHash appliedTallyValidator

    tallyNft :: Value
    tallyNft = Value.singleton tallySymbol tallyTokenName one

    indexNft :: Value
    indexNft =
      let
        txOut = (unwrap indexUtxoTxOutRefScript).output
      in
        (unwrap txOut).amount

    lookups :: Lookups.ScriptLookups
    lookups =
      mconcat
        [ Lookups.mintingPolicy appliedTallyPolicy
        , Lookups.unspentOutputs $ Map.singleton indexUtxoTxInput
            indexUtxoTxOutRefScript
        ]

    constraints :: Constraints.TxConstraints
    constraints =
      mconcat
        [ Constraints.mustMintValue tallyNft
        , Constraints.mustPayToScript
            tallyValidatorHash
            (Datum $ toData tallyStateDatum)
            Constraints.DatumInline
            tallyNft
        , Constraints.mustSpendScriptOutput indexUtxoTxInput unitRedeemer
        , Constraints.mustPayToScript
            indexValidatorHash
            (Datum $ toData updatedIndexDatum)
            Constraints.DatumInline
            indexNft
        , Constraints.mustReferenceOutput configUtxoTxInput
        ]

  txHash <- submitTxFromConstraints lookups constraints

  pure (txHash /\ tallySymbol)
  where
  incrementIndexDatum :: IndexNftDatum -> IndexNftDatum
  incrementIndexDatum (IndexNftDatum { index: oldIndex }) =
    IndexNftDatum { index: oldIndex + (fromInt 1) }

  mkTallyTokenName :: IndexNftDatum -> Maybe TokenName
  mkTallyTokenName (IndexNftDatum index) = mkTokenName $ show index

  mkValidatorConfig :: TallyNftConfig -> ConfigurationValidatorConfig
  mkValidatorConfig
    (TallyNftConfig { tncConfigNftCurrencySymbol, tncConfigNftTokenName }) =
    ConfigurationValidatorConfig
      { cvcConfigNftCurrencySymbol: tncConfigNftCurrencySymbol
      , cvcConfigNftTokenName: tncConfigNftTokenName
      }

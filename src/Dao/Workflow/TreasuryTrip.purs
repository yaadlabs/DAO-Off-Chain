{-|
Module: Dao.Workflow.TreasuryTrip
Description: Contract for disbursing treasury funds based on a trip proposal
-}
module Dao.Workflow.TreasuryTrip (treasuryTrip) where

import Contract.Log (logInfo')
import Contract.Monad (Contract)
import Contract.PlutusData (unitDatum)
import Contract.Prelude
  ( bind
  , discard
  , mconcat
  , pure
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts (Validator, ValidatorHash, validatorHash)
import Contract.Transaction
  ( TransactionHash
  , submitTxFromConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Value
  ( CurrencySymbol
  , TokenName
  )
import Dao.Component.Config.Params (mkValidatorConfig)
import Dao.Component.Config.Query (ConfigInfo, referenceConfigUtxo)
import Dao.Component.Tally.Query (TallyInfo, referenceTallyUtxo)
import Dao.Component.Treasury.Params (TreasuryParamsTrip)
import Dao.Component.Treasury.Query (TreasuryInfo, spendTreasuryUtxo)
import LambdaBuffers.ApplicationTypes.Proposal (ProposalType(ProposalType'Trip))
import Scripts.ConfigValidator (unappliedConfigValidator)
import Scripts.TallyValidator (unappliedTallyValidator)
import Scripts.TreasuryValidator (unappliedTreasuryValidator)

-- | Contract for disbursing treasury funds based on a trip proposal
treasuryTrip ::
  TreasuryParamsTrip ->
  CurrencySymbol ->
  CurrencySymbol ->
  CurrencySymbol ->
  TokenName ->
  Contract TransactionHash
treasuryTrip
  treasuryParams
  configSymbol
  tallySymbol
  treasurySymbol
  configTokenName = do
  logInfo' "Entering treasuryTrip transaction"

  -- Make the scripts
  let validatorConfig = mkValidatorConfig configSymbol configTokenName
  appliedTreasuryValidator :: Validator <- unappliedTreasuryValidator
    validatorConfig
  appliedTallyValidator :: Validator <- unappliedTallyValidator validatorConfig
  appliedConfigValidator :: Validator <- unappliedConfigValidator
    validatorConfig

  -- Query the UTXOs
  configInfo :: ConfigInfo <- referenceConfigUtxo configSymbol
    appliedConfigValidator
  tallyInfo :: TallyInfo <- referenceTallyUtxo tallySymbol appliedTallyValidator
  treasuryInfo :: TreasuryInfo <-
    spendTreasuryUtxo
      ( ProposalType'Trip
          treasuryParams.travelAgentAddress
          treasuryParams.travellerAddress
          treasuryParams.totalCost
      )
      treasurySymbol
      appliedTreasuryValidator

  let
    treasuryValidatorHash :: ValidatorHash
    treasuryValidatorHash = validatorHash appliedTreasuryValidator

    lookups :: Lookups.ScriptLookups
    lookups =
      mconcat
        [ configInfo.lookups
        , tallyInfo.lookups
        , treasuryInfo.lookups
        ]

    constraints :: Constraints.TxConstraints
    constraints =
      mconcat
        [ Constraints.mustPayToScript
            treasuryValidatorHash
            unitDatum
            Constraints.DatumInline
            treasuryInfo.value
        , treasuryInfo.constraints
        , configInfo.constraints
        , tallyInfo.constraints
        ]

  txHash <- submitTxFromConstraints lookups constraints

  pure txHash

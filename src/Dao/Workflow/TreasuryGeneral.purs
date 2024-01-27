{-|
Module: Dao.Workflow.TreasuryGeneral
Description: Contract for disbursing treasury funds based on a general proposal
-}
module Dao.Workflow.TreasuryGeneral (treasuryGeneral) where

import Contract.Address (Address, PaymentPubKeyHash)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData (unitDatum)
import Contract.Prelude
  ( bind
  , discard
  , mconcat
  , min
  , pure
  , unwrap
  , (#)
  , ($)
  , (*)
  , (+)
  , (/)
  , (>=)
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts (Validator, ValidatorHash, validatorHash)
import Contract.Transaction
  ( TransactionHash
  , submitTxFromConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Value
  ( Value
  , adaSymbol
  , adaToken
  , singleton
  )
import Dao.Component.Config.Params (mkValidatorConfig)
import Dao.Component.Config.Query (ConfigInfo, referenceConfigUtxo)
import Dao.Component.Tally.Query (TallyInfo, referenceTallyUtxo)
import Dao.Component.Treasury.Params (TreasuryParams)
import Dao.Component.Treasury.Query (TreasuryInfo, spendTreasuryUtxo)
import Dao.Scripts.Validator.Config (unappliedConfigValidatorDebug)
import Dao.Scripts.Validator.Tally (unappliedTallyValidatorDebug)
import Dao.Scripts.Validator.Treasury (unappliedTreasuryValidatorDebug)
import Dao.Utils.Address (addressToPaymentPubKeyHash)
import Dao.Utils.Error (guardContract)
import Dao.Utils.Value (allPositive, normaliseValue, valueSubtraction)
import Data.Maybe (Maybe(Nothing, Just))
import JS.BigInt (BigInt, fromInt)
import LambdaBuffers.ApplicationTypes.Configuration (DynamicConfigDatum)
import LambdaBuffers.ApplicationTypes.Proposal
  ( ProposalType(ProposalType'General)
  )
import LambdaBuffers.ApplicationTypes.Tally (TallyStateDatum)

-- | Contract for disbursing treasury funds based on a general proposal
treasuryGeneral ::
  TreasuryParams ->
  Contract TransactionHash
treasuryGeneral params' = do
  logInfo' "Entering treasuryGeneral transaction"

  let params = params' # unwrap

  -- Make the scripts
  let
    validatorConfig = mkValidatorConfig params.configSymbol
      params.configTokenName
  appliedTreasuryValidator :: Validator <- unappliedTreasuryValidatorDebug
    validatorConfig
  appliedTallyValidator :: Validator <- unappliedTallyValidatorDebug
    validatorConfig
  appliedConfigValidator :: Validator <- unappliedConfigValidatorDebug
    validatorConfig

  -- Query the UTXOs
  configInfo :: ConfigInfo <- referenceConfigUtxo params.configSymbol
    appliedConfigValidator
  tallyInfo :: TallyInfo <- referenceTallyUtxo params.tallySymbol
    appliedTallyValidator
  treasuryInfo :: TreasuryInfo <-
    spendTreasuryUtxo
      params.treasurySymbol
      appliedTreasuryValidator

  let
    configDatum :: DynamicConfigDatum
    configDatum = configInfo.datum

    tallyDatum :: TallyStateDatum
    tallyDatum = tallyInfo.datum

  -- Get the treasury payment info from the 'TallyStateDatum'
  paymentAddress :: Address <- liftContractM "Not a general proposal" $
    getPaymentAddress tallyDatum
  paymentAmount :: BigInt <- liftContractM "Not a general proposal" $
    getPaymentAmount tallyDatum
  paymentKey :: PaymentPubKeyHash <-
    liftContractM "Could not convert address to key" $
      addressToPaymentPubKeyHash paymentAddress

  let
    votesFor :: BigInt
    votesFor = tallyDatum # unwrap # _.for

    votesAgainst :: BigInt
    votesAgainst = tallyDatum # unwrap # _.against

    totalVotes :: BigInt
    totalVotes = votesFor + votesAgainst

    configTotalVotes :: BigInt
    configTotalVotes = configDatum # unwrap # _.totalVotes

    relativeMajority :: BigInt
    relativeMajority = (totalVotes * (fromInt 1000)) / configTotalVotes

    majorityPercent :: BigInt
    majorityPercent = (votesFor * (fromInt 1000)) / totalVotes

    configGeneralRelativeMajorityPercent :: BigInt
    configGeneralRelativeMajorityPercent = configDatum # unwrap #
      _.generalRelativeMajorityPercent

    configGeneralMajorityPercent :: BigInt
    configGeneralMajorityPercent = configDatum # unwrap #
      _.generalMajorityPercent

    configMaxGeneralDisbursement :: BigInt
    configMaxGeneralDisbursement = configDatum # unwrap #
      _.maxGeneralDisbursement

    disbursementAmount :: BigInt
    disbursementAmount = min configMaxGeneralDisbursement paymentAmount

    treasuryInputAmount :: Value
    treasuryInputAmount = treasuryInfo.value

    amountToSendToPaymentAddress :: Value
    amountToSendToPaymentAddress = singleton adaSymbol adaToken
      disbursementAmount

    amountToSendBackToTreasury :: Value
    amountToSendBackToTreasury = normaliseValue
      (valueSubtraction treasuryInputAmount amountToSendToPaymentAddress)

  -- Check that the treasury input amount covers the payment amount
  guardContract "Not enough treasury funds to cover payment" $ allPositive
    amountToSendBackToTreasury

  -- Check for sufficient votes
  guardContract "Relative majority is too low" $ relativeMajority >=
    configGeneralRelativeMajorityPercent
  guardContract "Majority percent is too low" $ majorityPercent >=
    configGeneralMajorityPercent

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
            amountToSendBackToTreasury
        , Constraints.mustPayToPubKey paymentKey amountToSendToPaymentAddress
        , treasuryInfo.constraints
        , configInfo.constraints
        , tallyInfo.constraints
        ]

  txHash <- submitTxFromConstraints lookups constraints

  pure txHash
  where
  getPaymentAddress :: TallyStateDatum -> Maybe Address
  getPaymentAddress tallyDatum =
    let
      proposalType = tallyDatum # unwrap # _.proposal
    in
      case proposalType of
        (ProposalType'General address _) -> Just address
        _ -> Nothing

  getPaymentAmount :: TallyStateDatum -> Maybe BigInt
  getPaymentAmount tallyDatum =
    let
      proposalType = tallyDatum # unwrap # _.proposal
    in
      case proposalType of
        (ProposalType'General _ amount) -> Just amount
        _ -> Nothing

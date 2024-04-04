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
  , zero
  , (#)
  , ($)
  , (*)
  , (+)
  , (/)
  , (>)
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
import Dao.Scripts.Validator
  ( unappliedConfigValidator
  , unappliedTallyValidator
  , unappliedTreasuryValidator
  )
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
  appliedTreasuryValidator :: Validator <- unappliedTreasuryValidator
    validatorConfig
  appliedTallyValidator :: Validator <- unappliedTallyValidator
    validatorConfig
  appliedConfigValidator :: Validator <- unappliedConfigValidator
    validatorConfig

  -- Query the UTXOs
  configInfo :: ConfigInfo <- referenceConfigUtxo params.configSymbol
    appliedConfigValidator
  tallyInfo :: TallyInfo <- referenceTallyUtxo params.tallySymbol
    params.proposalTokenName
    appliedTallyValidator
  treasuryInfo :: TreasuryInfo <-
    spendTreasuryUtxo
      params.treasurySymbol
      appliedTreasuryValidator

  let
    -- The main config referenced at the config UTXO
    configDatum :: DynamicConfigDatum
    configDatum = configInfo.datum

    -- The tally datum referenced at the tally UTXO
    tallyDatum :: TallyStateDatum
    tallyDatum = tallyInfo.datum

  {- Get the treasury payment info from the 'TallyStateDatum' -}
  -- The address of the user that will receive the payment
  paymentAddress :: Address <- liftContractM "Not a general proposal" $
    getPaymentAddress tallyDatum
  -- The amount of Ada the user should receive
  paymentAmount :: BigInt <- liftContractM "Not a general proposal" $
    getPaymentAmount tallyDatum
  -- The PKH of the user
  paymentKey :: PaymentPubKeyHash <-
    liftContractM "Could not convert address to key" $
      addressToPaymentPubKeyHash paymentAddress

  let
    -- The number of votes cast in favour of the proposal
    votesFor :: BigInt
    votesFor = tallyDatum # unwrap # _.for

    -- The number of votes cast in opposition to the proposal
    votesAgainst :: BigInt
    votesAgainst = tallyDatum # unwrap # _.against

    totalVotes :: BigInt
    totalVotes = votesFor + votesAgainst

    -- Set in 'createConfig' tx, must not be zero
    configTotalVotes :: BigInt
    configTotalVotes = configDatum # unwrap # _.totalVotes

    -- Calculates the 'relative majority' based on on-chain script requirements
    -- This value must exceed 'configGeneralRelativeMajorityPercent' threshold
    -- set in the 'DynamicConfigDatum'
    relativeMajority :: BigInt
    relativeMajority = (totalVotes * (fromInt 1000)) / configTotalVotes

    -- Calculates the 'majority' based on on-chain script requirements
    -- This value must exceed 'configGeneralMajorityPercent' threshold
    -- set in the 'DynamicConfigDatum'
    majorityPercent :: BigInt
    majorityPercent = (votesFor * (fromInt 1000)) / totalVotes

    -- Get the 'configGeneralRelativeMajorityPercent' threshold from the config
    configGeneralRelativeMajorityPercent :: BigInt
    configGeneralRelativeMajorityPercent = configDatum # unwrap #
      _.generalRelativeMajorityPercent

    -- Get the 'configGeneralMajorityPercent' threshold from the config
    configGeneralMajorityPercent :: BigInt
    configGeneralMajorityPercent = configDatum # unwrap #
      _.generalMajorityPercent

    -- A max threshold for the disbursement amount
    configMaxGeneralDisbursement :: BigInt
    configMaxGeneralDisbursement = configDatum # unwrap #
      _.maxGeneralDisbursement

    -- The disbursement amount, it cannot be greater than the 'configMaxGeneralDisbursement'
    disbursementAmount :: BigInt
    disbursementAmount = min configMaxGeneralDisbursement paymentAmount

    -- The value held at the treasury input UTXO which
    -- must cover the disbursement amount
    treasuryInputAmount :: Value
    treasuryInputAmount = treasuryInfo.value

    -- The Ada amount to send to the receiver
    amountToSendToPaymentAddress :: Value
    amountToSendToPaymentAddress = singleton adaSymbol adaToken
      disbursementAmount

    -- The change to send back to the treasury
    amountToSendBackToTreasury :: Value
    amountToSendBackToTreasury = normaliseValue
      (valueSubtraction treasuryInputAmount amountToSendToPaymentAddress)

  -- Check that the treasury input amount covers the payment amount
  guardContract "Not enough treasury funds to cover payment" $ allPositive
    amountToSendBackToTreasury

  -- Prevents an on-chain divide by zero error in the case that
  -- configTotalVotes was set to zero
  guardContract
    "The totalVotes field of the config datum must be greater than zero"
    $ configTotalVotes
    > zero

  -- Check for sufficient votes by ensuring the config thresholds are exceeded
  guardContract "Relative majority is too low" $ relativeMajority >=
    configGeneralRelativeMajorityPercent
  guardContract "Majority percent is too low" $ majorityPercent >=
    configGeneralMajorityPercent

  let
    -- We need to send change back to the tresaury
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
        -- ^ Send the change back to the treasury
        , Constraints.mustPayToPubKey paymentKey amountToSendToPaymentAddress
        -- ^ Send the Ada to the user's key corresponding to
        -- the payment address specified in the tally datum
        , treasuryInfo.constraints
        , configInfo.constraints
        , tallyInfo.constraints
        ]

  txHash <- submitTxFromConstraints lookups constraints

  pure txHash
  where
  -- Get the user's address
  getPaymentAddress :: TallyStateDatum -> Maybe Address
  getPaymentAddress tallyDatum =
    let
      proposalType = tallyDatum # unwrap # _.proposal
    in
      case proposalType of
        (ProposalType'General address _) -> Just address
        _ -> Nothing

  -- Get the amount of Ada to send to the user
  getPaymentAmount :: TallyStateDatum -> Maybe BigInt
  getPaymentAmount tallyDatum =
    let
      proposalType = tallyDatum # unwrap # _.proposal
    in
      case proposalType of
        (ProposalType'General _ amount) -> Just amount
        _ -> Nothing

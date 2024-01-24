{-|
Module: Dao.Workflow.TreasuryTrip
Description: Contract for disbursing treasury funds based on a trip proposal
-}
module Dao.Workflow.TreasuryTrip (treasuryTrip) where

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
  , (-)
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
  ( CurrencySymbol
  , TokenName
  , Value
  , adaSymbol
  , adaToken
  , singleton
  )
import Dao.Component.Config.Params (mkValidatorConfig)
import Dao.Component.Config.Query (ConfigInfo, referenceConfigUtxo)
import Dao.Component.Tally.Query (TallyInfo, referenceTallyUtxo)
import Dao.Component.Treasury.Params (TreasuryParams)
import Dao.Component.Treasury.Query (TreasuryInfo, spendTreasuryUtxo)
import Dao.Utils.Address (addressToPaymentPubKeyHash)
import Dao.Utils.Error (guardContract)
import Dao.Utils.Value (allPositive, normaliseValue, valueSubtraction)
import Data.Maybe (Maybe(Just, Nothing))
import JS.BigInt (BigInt, fromInt)
import LambdaBuffers.ApplicationTypes.Configuration (DynamicConfigDatum)
import LambdaBuffers.ApplicationTypes.Proposal (ProposalType(ProposalType'Trip))
import LambdaBuffers.ApplicationTypes.Tally (TallyStateDatum)
import Scripts.ConfigValidator (unappliedConfigValidatorDebug)
import Scripts.TallyValidator (unappliedTallyValidatorDebug)
import Scripts.TreasuryValidator (unappliedTreasuryValidatorDebug)

-- | Contract for disbursing treasury funds based on a trip proposal
treasuryTrip :: TreasuryParams -> Contract TransactionHash
treasuryTrip params = do
  logInfo' "Entering treasuryTrip transaction"

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
    dynamicConfig :: DynamicConfigDatum
    dynamicConfig = configInfo.datum

    tallyDatum :: TallyStateDatum
    tallyDatum = tallyInfo.datum

  -- Get the treasury payment info from the 'TallyStateDatum'
  travelAgentAddress :: Address <- liftContractM "Not a trip proposal" $
    getTravelAgentAddress tallyDatum
  travellerAddress :: Address <- liftContractM "Not a trip proposal" $
    getTravellerAddress tallyDatum
  totalTravelCost :: BigInt <- liftContractM "Not a trip proposal" $
    getTravelCost tallyDatum

  travelAgentPaymentKey :: PaymentPubKeyHash <-
    liftContractM "Could not convert address to key" $
      addressToPaymentPubKeyHash travelAgentAddress
  travellerPaymentKey :: PaymentPubKeyHash <-
    liftContractM "Could not convert address to key" $
      addressToPaymentPubKeyHash travellerAddress

  let
    votesFor :: BigInt
    votesFor = tallyDatum # unwrap # _.for

    votesAgainst :: BigInt
    votesAgainst = tallyDatum # unwrap # _.against

    totalVotes :: BigInt
    totalVotes = votesFor + votesAgainst

    configTotalVotes :: BigInt
    configTotalVotes = dynamicConfig # unwrap # _.totalVotes

    relativeMajority :: BigInt
    relativeMajority = (totalVotes * (fromInt 1000)) / configTotalVotes

    majorityPercent :: BigInt
    majorityPercent = (votesFor * (fromInt 1000)) / totalVotes

    configTripRelativeMajorityPercent :: BigInt
    configTripRelativeMajorityPercent = dynamicConfig # unwrap #
      _.tripRelativeMajorityPercent

    configTripMajorityPercent :: BigInt
    configTripMajorityPercent = dynamicConfig # unwrap # _.tripMajorityPercent

    configMaxTripDisbursement :: BigInt
    configMaxTripDisbursement = dynamicConfig # unwrap # _.maxTripDisbursement

    configAgentDisbursementPercent :: BigInt
    configAgentDisbursementPercent = dynamicConfig # unwrap #
      _.agentDisbursementPercent

    disbursementAmount :: BigInt
    disbursementAmount = min configMaxTripDisbursement totalTravelCost

    disbursementAmountLovelaces :: Value
    disbursementAmountLovelaces = singleton adaSymbol adaToken
      disbursementAmount

    treasuryInputAmount :: Value
    treasuryInputAmount = treasuryInfo.value

    amountToSendBackToTreasuryLovelaces :: Value
    amountToSendBackToTreasuryLovelaces = normaliseValue
      (valueSubtraction treasuryInputAmount disbursementAmountLovelaces)

    amountToSendToTravelAgent :: BigInt
    amountToSendToTravelAgent =
      (totalTravelCost * configAgentDisbursementPercent) / (fromInt 1000)

    amountToSendToTraveller :: BigInt
    amountToSendToTraveller = totalTravelCost - amountToSendToTravelAgent

    amountToSendToTravelAgentLovelaces :: Value
    amountToSendToTravelAgentLovelaces = singleton adaSymbol adaToken
      amountToSendToTravelAgent

    amountToSendToTravellerLovelaces :: Value
    amountToSendToTravellerLovelaces = singleton adaSymbol adaToken
      amountToSendToTraveller

  -- Check that the treasury input amount covers the payment amount
  guardContract "Not enough treasury funds to cover payment" $ allPositive
    amountToSendBackToTreasuryLovelaces

  -- Check for sufficient votes
  guardContract "Relative majority is too low" $ relativeMajority >=
    configTripRelativeMajorityPercent
  guardContract "Majority percent is too low" $ majorityPercent >=
    configTripMajorityPercent

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
            amountToSendBackToTreasuryLovelaces
        , Constraints.mustPayToPubKey
            travelAgentPaymentKey
            amountToSendToTravellerLovelaces
        , Constraints.mustPayToPubKey
            travellerPaymentKey
            amountToSendToTravelAgentLovelaces
        , treasuryInfo.constraints
        , configInfo.constraints
        , tallyInfo.constraints
        ]

  txHash <- submitTxFromConstraints lookups constraints

  pure txHash
  where
  getTravelAgentAddress :: TallyStateDatum -> Maybe Address
  getTravelAgentAddress tallyDatum =
    let
      proposalType = tallyDatum # unwrap # _.proposal
    in
      case proposalType of
        (ProposalType'Trip address _ _) -> Just address
        _ -> Nothing

  getTravellerAddress :: TallyStateDatum -> Maybe Address
  getTravellerAddress tallyDatum =
    let
      proposalType = tallyDatum # unwrap # _.proposal
    in
      case proposalType of
        (ProposalType'Trip _ address _) -> Just address
        _ -> Nothing

  getTravelCost :: TallyStateDatum -> Maybe BigInt
  getTravelCost tallyDatum =
    let
      proposalType = tallyDatum # unwrap # _.proposal
    in
      case proposalType of
        (ProposalType'Trip _ _ amount) -> Just amount
        _ -> Nothing

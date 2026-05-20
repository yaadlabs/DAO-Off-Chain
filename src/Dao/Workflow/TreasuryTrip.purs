{-|
Module: Dao.Workflow.TreasuryTrip
Description: Contract for disbursing treasury funds based on a trip proposal
-}
module Dao.Workflow.TreasuryTrip (treasuryTrip) where

import Cardano.Plutus.Types.Address (Address) as Plutus
import Cardano.Plutus.Types.Address (toCardano) as Plutus.Address
import Cardano.Types.BigNum (fromBigInt) as BigNum
import Cardano.Types.Value (lovelaceValueOf)
import Contract.Address (Address, PaymentPubKeyHash, getNetworkId)
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
import Contract.Transaction (TransactionHash, submitTxFromConstraints)
import Contract.TxConstraints as Constraints
import Contract.Value (Value)
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
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import JS.BigInt (BigInt, fromInt)
import LambdaBuffers.ApplicationTypes.Configuration (DynamicConfigDatum)
import LambdaBuffers.ApplicationTypes.Proposal (ProposalType(ProposalType'Trip))
import LambdaBuffers.ApplicationTypes.Tally (TallyStateDatum)
import Partial.Unsafe (unsafePartial)

-- | Contract for disbursing treasury funds based on a trip proposal
treasuryTrip :: TreasuryParams -> Contract TransactionHash
treasuryTrip params' = do
  logInfo' "Entering treasuryTrip transaction"

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
    dynamicConfig :: DynamicConfigDatum
    dynamicConfig = configInfo.datum

    -- Get the treasury payment info from the 'TallyStateDatum'
    tallyDatum :: TallyStateDatum
    tallyDatum = tallyInfo.datum

  network <- getNetworkId

  -- Get the treasury payment info from the 'TallyStateDatum'
  (travelAgentAddress :: Address) <- do
    paddr <- liftContractM "Not a trip proposal" $ getTravelAgentAddress
      tallyDatum
    liftContractM "Could not convert travel agent address" $
      Plutus.Address.toCardano network paddr

  (travellerAddress :: Address) <- do
    paddr <- liftContractM "Not a trip proposal" $ getTravellerAddress
      tallyDatum
    liftContractM "Could not convert traveller address" $
      Plutus.Address.toCardano network paddr

  totalTravelCost :: BigInt <- liftContractM "Not a trip proposal" $
    getTravelCost tallyDatum

  travelAgentPaymentKey :: PaymentPubKeyHash <-
    liftContractM "Could not convert address to key" $
      addressToPaymentPubKeyHash travelAgentAddress
  travellerPaymentKey :: PaymentPubKeyHash <-
    liftContractM "Could not convert address to key" $
      addressToPaymentPubKeyHash travellerAddress

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
    configTotalVotes = dynamicConfig # unwrap # _.totalVotes

    -- Calculates the 'relative majority' based on on-chain script requirements
    -- This value must exceed 'configTripRelativeMajorityPercent' threshold
    -- set in the 'DynamicConfigDatum'
    relativeMajority :: BigInt
    relativeMajority = (totalVotes * (fromInt 1000)) / configTotalVotes

    -- Calculates the 'majority' based on on-chain script requirements
    -- This value must exceed 'configTripMajorityPercent' threshold
    -- set in the 'DynamicConfigDatum'
    majorityPercent :: BigInt
    majorityPercent = (votesFor * (fromInt 1000)) / totalVotes

    -- Get the 'configTripRelativeMajorityPercent' threshold from the config
    configTripRelativeMajorityPercent :: BigInt
    configTripRelativeMajorityPercent = dynamicConfig # unwrap #
      _.tripRelativeMajorityPercent

    -- Get the 'configTripRelativeMajorityPercent' threshold from the config
    configTripMajorityPercent :: BigInt
    configTripMajorityPercent = dynamicConfig # unwrap # _.tripMajorityPercent

    -- A max threshold for the disbursement amount
    configMaxTripDisbursement :: BigInt
    configMaxTripDisbursement = dynamicConfig # unwrap # _.maxTripDisbursement

    -- Get the amount to send to the travel agent's address
    configAgentDisbursementPercent :: BigInt
    configAgentDisbursementPercent = dynamicConfig # unwrap #
      _.agentDisbursementPercent

    -- Get the total cost, which cannot exceed the max threshold specified
    disbursementAmount :: BigInt
    disbursementAmount = min configMaxTripDisbursement totalTravelCost

    disbursementAmountLovelaces :: Value
    disbursementAmountLovelaces =
      -- FIXME: unsafe
      lovelaceValueOf $ unsafePartial fromJust $ BigNum.fromBigInt
        disbursementAmount

    -- The value held at the treasury input UTXO which
    -- must cover the disbursement amount
    treasuryInputAmount :: Value
    treasuryInputAmount = treasuryInfo.value

    -- The change to send back to the treasury
    amountToSendBackToTreasuryLovelaces :: Value
    amountToSendBackToTreasuryLovelaces =
      -- FIXME: unsafe
      normaliseValue $ unsafePartial fromJust $
        valueSubtraction treasuryInputAmount disbursementAmountLovelaces

    -- Caluclate amount to send to the travel agent
    amountToSendToTravelAgent :: BigInt
    amountToSendToTravelAgent =
      (totalTravelCost * configAgentDisbursementPercent) / (fromInt 1000)

    -- Caluclate the amount to send to the traveller
    amountToSendToTraveller :: BigInt
    amountToSendToTraveller = totalTravelCost - amountToSendToTravelAgent

    amountToSendToTravelAgentLovelaces :: Value
    amountToSendToTravelAgentLovelaces =
      -- FIXME: unsafe
      lovelaceValueOf $ unsafePartial fromJust $ BigNum.fromBigInt
        amountToSendToTravelAgent

    amountToSendToTravellerLovelaces :: Value
    amountToSendToTravellerLovelaces =
      -- FIXME: unsafe
      lovelaceValueOf $ unsafePartial fromJust $ BigNum.fromBigInt
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
        -- ^ Pay the change back to the treasury
        , Constraints.mustPayToPubKey
            travellerPaymentKey
            amountToSendToTravellerLovelaces
        -- Pay the traveller their share
        , Constraints.mustPayToPubKey
            travelAgentPaymentKey
            amountToSendToTravelAgentLovelaces
        -- Pay the travel agent their share
        , treasuryInfo.constraints
        , configInfo.constraints
        , tallyInfo.constraints
        ]

  txHash <- submitTxFromConstraints lookups constraints

  pure txHash
  where
  -- Get the travel agent's address from the tally datum
  getTravelAgentAddress :: TallyStateDatum -> Maybe Plutus.Address
  getTravelAgentAddress tallyDatum =
    let
      proposalType = tallyDatum # unwrap # _.proposal
    in
      case proposalType of
        (ProposalType'Trip address _ _) -> Just address
        _ -> Nothing

  -- Get the traveller's address from the tally datum
  getTravellerAddress :: TallyStateDatum -> Maybe Plutus.Address
  getTravellerAddress tallyDatum =
    let
      proposalType = tallyDatum # unwrap # _.proposal
    in
      case proposalType of
        (ProposalType'Trip _ address _) -> Just address
        _ -> Nothing

  -- Get the total cost of the disbursement from the tally datum
  getTravelCost :: TallyStateDatum -> Maybe BigInt
  getTravelCost tallyDatum =
    let
      proposalType = tallyDatum # unwrap # _.proposal
    in
      case proposalType of
        (ProposalType'Trip _ _ amount) -> Just amount
        _ -> Nothing

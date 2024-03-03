module Test.Data.Tally
  ( sampleGeneralProposalTallyStateDatum
  , sampleTripProposalTallyStateDatum
  , sampleUpgradeConfigProposalTallyStateDatum
  ) where

import Contract.Address (Address)
import Contract.Monad (Contract)
import Contract.Prelude (bind, pure, ($))
import Contract.Prelude ((*), (+))
import Contract.Time (POSIXTime(POSIXTime))
import Contract.Value (CurrencySymbol)
import Dao.Utils.Time (getCurrentTime, mkPosixTime)
import JS.BigInt as BigInt
import LambdaBuffers.ApplicationTypes.Proposal
  ( ProposalType(ProposalType'General)
  , ProposalType(ProposalType'Trip)
  , ProposalType(ProposalType'Upgrade)
  )
import LambdaBuffers.ApplicationTypes.Tally (TallyStateDatum(TallyStateDatum))

sampleGeneralProposalTallyStateDatum :: Address -> Contract TallyStateDatum
sampleGeneralProposalTallyStateDatum paymentAddress = do
  currentTime <- getCurrentTime
  let endTime = currentTime + offsetPosixTime
  pure $
    TallyStateDatum
      { proposal: ProposalType'General paymentAddress
          (BigInt.fromInt 20_000_000)
      , proposalEndTime: endTime
      , for: BigInt.fromInt 0
      , against: BigInt.fromInt 0
      }

sampleTripProposalTallyStateDatum ::
  Address -> Address -> Contract TallyStateDatum
sampleTripProposalTallyStateDatum travelAgentAddress travellerAddress = do
  currentTime <- getCurrentTime
  let endTime = currentTime + offsetPosixTime
  pure $ TallyStateDatum
    { proposal: ProposalType'Trip travelAgentAddress travellerAddress
        (BigInt.fromInt 10_000_000)
    , proposalEndTime: endTime
    , for: BigInt.fromInt 0
    , against: BigInt.fromInt 0
    }

sampleUpgradeConfigProposalTallyStateDatum :: CurrencySymbol -> TallyStateDatum
sampleUpgradeConfigProposalTallyStateDatum symbol =
  TallyStateDatum
    { proposal: ProposalType'Upgrade symbol
    , proposalEndTime: proposalEndTimeWayInFuture
    , for: BigInt.fromInt 0
    , against: BigInt.fromInt 0
    }

proposalEndTimeWayInFuture :: POSIXTime
proposalEndTimeWayInFuture = mkPosixTime "1795941991500"

offsetPosixTime :: POSIXTime
offsetPosixTime = mkPosixTime "0000000040000"

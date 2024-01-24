module Test.Data.Tally
  ( sampleGeneralProposalTallyStateDatum
  , sampleTripProposalTallyStateDatum
  ) where

import Contract.Address (Address)
import Contract.Monad (Contract)
import Contract.Prelude (bind, pure, ($))
import Contract.Time (POSIXTime(POSIXTime))
import Dao.Utils.Time (mkPosixTime)
import JS.BigInt as BigInt
import LambdaBuffers.ApplicationTypes.Proposal
  ( ProposalType(ProposalType'General)
  , ProposalType(ProposalType'Trip)
  )
import LambdaBuffers.ApplicationTypes.Tally (TallyStateDatum(TallyStateDatum))

sampleGeneralProposalTallyStateDatum :: Address -> TallyStateDatum
sampleGeneralProposalTallyStateDatum paymentAddress =
  TallyStateDatum
    { proposal: ProposalType'General paymentAddress (BigInt.fromInt 20_000_000)
    , proposalEndTime: proposalEndTimeWayInFuture
    , for: BigInt.fromInt 0
    , against: BigInt.fromInt 0
    }

sampleTripProposalTallyStateDatum :: Address -> Address -> TallyStateDatum
sampleTripProposalTallyStateDatum travelAgentAddress travellerAddress =
  TallyStateDatum
    { proposal: ProposalType'Trip travelAgentAddress travellerAddress
        (BigInt.fromInt 10_000_000)
    , proposalEndTime: proposalEndTimeWayInFuture
    , for: BigInt.fromInt 0
    , against: BigInt.fromInt 0
    }

proposalEndTimeWayInFuture :: POSIXTime
proposalEndTimeWayInFuture = mkPosixTime "1795941991500"

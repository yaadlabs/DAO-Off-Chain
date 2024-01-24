module Test.Data.Tally (sampleTallyStateDatum) where

import Contract.Address (Address)
import Contract.Monad (Contract)
import Contract.Prelude (bind, pure, ($))
import Contract.Time (POSIXTime(POSIXTime))
import Dao.Utils.Time (mkPosixTime)
import JS.BigInt as BigInt
import LambdaBuffers.ApplicationTypes.Proposal
  ( ProposalType(ProposalType'General)
  )
import LambdaBuffers.ApplicationTypes.Tally (TallyStateDatum(TallyStateDatum))

sampleTallyStateDatum :: Address -> TallyStateDatum
sampleTallyStateDatum paymentAddress =
  TallyStateDatum
    { proposal: ProposalType'General paymentAddress (BigInt.fromInt 1_000_000)
    , proposalEndTime: proposalEndTimeWayInFuture
    , for: BigInt.fromInt 0
    , against: BigInt.fromInt 0
    }

proposalEndTimeWayInFuture :: POSIXTime
proposalEndTimeWayInFuture = mkPosixTime "1795941991500"

module Test.Data.Tally (sampleTallyStateDatum) where

import Contract.Monad (Contract)
import Contract.Prelude (bind, pure, ($))
import Contract.Time (POSIXTime(POSIXTime))
import JS.BigInt as BigInt
import LambdaBuffers.ApplicationTypes.Proposal
  ( ProposalType(ProposalType'General)
  )
import LambdaBuffers.ApplicationTypes.Tally (TallyStateDatum(TallyStateDatum))
import Test.Data.Address (dummyAddress)

sampleTallyStateDatum :: Contract TallyStateDatum
sampleTallyStateDatum = do
  dummyAddress' <- dummyAddress
  pure $
    TallyStateDatum
      { proposal: ProposalType'General dummyAddress' (BigInt.fromInt 10)
      , proposalEndTime: POSIXTime $ BigInt.fromInt 0
      , for: BigInt.fromInt 0
      , against: BigInt.fromInt 0
      }

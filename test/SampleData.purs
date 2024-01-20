module Test.SampleData where

import Contract.Value (adaSymbol, adaToken)
import Dao.Components.Config.Params (ConfigParams)
import JS.BigInt as BigInt

sampleConfigParams :: ConfigParams
sampleConfigParams =
  { configTokenName: adaToken
  , upgradeMajorityPercent: BigInt.fromInt 0
  , upgradeRelativeMajorityPercent: BigInt.fromInt 0
  , generalMajorityPercent: BigInt.fromInt 0
  , generalRelativeMajorityPercent: BigInt.fromInt 0
  , tripMajorityPercent: BigInt.fromInt 0
  , tripRelativeMajorityPercent: BigInt.fromInt 0
  , totalVotes: BigInt.fromInt 0
  , maxGeneralDisbursement: BigInt.fromInt 0
  , maxTripDisbursement: BigInt.fromInt 0
  , agentDisbursementPercent: BigInt.fromInt 0
  , proposalTallyEndOffset: BigInt.fromInt 0
  , tallyNft: adaSymbol
  , voteCurrencySymbol: adaSymbol
  , voteTokenName: adaToken
  , voteNft: adaSymbol
  , voteFungibleCurrencySymbol: adaSymbol
  , voteFungibleTokenName: adaToken
  , fungibleVotePercent: BigInt.fromInt 0
  }

module Test.Data.Config (dummyNewConfig) where

import Contract.Monad (Contract)
import Contract.Prelude (bind, pure, unwrap, (#), ($))
import Contract.Scripts (ScriptHash, validatorHash)
import Contract.Value (adaSymbol, adaToken)
import Dao.Component.Config.Params (ConfigParams)
import JS.BigInt as BigInt
import LambdaBuffers.ApplicationTypes.Configuration
  ( DynamicConfigDatum(DynamicConfigDatum)
  )
import Scripts.AlwaysSucceeds (alwaysSucceedsValidatorScript)

-- | Used for the 'new config' when running the 'upgradeConfig' workflow test
dummyNewConfig :: Contract DynamicConfigDatum
dummyNewConfig = do
  someScriptHash <- someScriptHash'
  pure $ DynamicConfigDatum
    { tallyValidator: someScriptHash
    , treasuryValidator: someScriptHash
    , configurationValidator: someScriptHash
    , voteValidator: someScriptHash
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

someScriptHash' :: Contract ScriptHash
someScriptHash' = do
  validator <- alwaysSucceedsValidatorScript
  pure $ validatorHash validator # unwrap

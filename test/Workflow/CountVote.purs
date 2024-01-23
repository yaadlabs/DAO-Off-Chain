{-|
Module: Test.Workflow.CountVote
Description: Test the count vote workflow
-}
module Test.Workflow.CountVote (suite) where

import Contract.Chain (waitNSlots)
import Contract.Log (logInfo')
import Contract.Numeric.Natural as Natural
import Contract.Prelude
  ( Unit
  , bind
  , discard
  , pure
  , show
  , show
  , unit
  , void
  , ($)
  , (/\)
  , (<>)
  )
import Contract.Test.Mote (TestPlanM)
import Contract.Test.Plutip
  ( InitialUTxOs
  , PlutipTest
  , withKeyWallet
  , withWallets
  )
import Contract.Transaction (awaitTxConfirmedWithTimeout)
import Contract.Value (adaSymbol, adaToken, scriptCurrencySymbol)
import Contract.Wallet (getWalletCollateral)
import Dao.Component.Config.Params (ConfigParams)
import Dao.Workflow.CountVote (countVote)
import Dao.Workflow.CreateConfig (createConfig)
import Dao.Workflow.CreateFungible (createFungible)
import Dao.Workflow.CreateIndex (createIndex)
import Dao.Workflow.CreateProposal (createProposal)
import Dao.Workflow.CreateVotePass (createVotePass)
import Dao.Workflow.VoteOnProposal (voteOnProposal)
import Data.Time.Duration (Seconds(Seconds))
import JS.BigInt (fromInt) as BigInt
import LambdaBuffers.ApplicationTypes.Vote (VoteDirection(VoteDirection'For))
import Mote (group, test)
import Scripts.VoteNft (voteNftPolicy)
import Test.Data.Config (sampleConfigParams)
import Test.Data.Tally (sampleTallyStateDatum)

suite :: TestPlanM PlutipTest Unit
suite = do
  group "DAO tests" do
    test "Count votes on proposal test" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 2_000_000_000
          , BigInt.fromInt 500_000_000
          ]
      withWallets distribution \wallet -> do
        withKeyWallet wallet do

          (votePassTxHash /\ votePassSymbol /\ votePassTokenName) <-
            createVotePass
          void $ awaitTxConfirmedWithTimeout (Seconds 600.0) votePassTxHash

          (fungibleTxHash /\ fungibleSymbol /\ fungibleTokenName) <-
            createFungible (BigInt.fromInt 400)
          void $ awaitTxConfirmedWithTimeout (Seconds 600.0) fungibleTxHash

          (createIndexTxHash /\ indexSymbol /\ indexTokenName) <-
            createIndex adaToken
          void $ awaitTxConfirmedWithTimeout (Seconds 600.0) createIndexTxHash

          let
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
              , voteTokenName: adaToken
              , voteFungibleCurrencySymbol: fungibleSymbol
              , voteFungibleTokenName: fungibleTokenName
              , fungibleVotePercent: BigInt.fromInt 10

              -- Index needed for making tallyNft
              , indexSymbol: indexSymbol
              , indexTokenName: indexTokenName
              }

          (createConfigTxHash /\ configSymbol /\ configTokenName) <-
            createConfig sampleConfigParams
          void $ awaitTxConfirmedWithTimeout (Seconds 600.0) createConfigTxHash

          sampleTallyStateDatum' <- sampleTallyStateDatum

          let
            proposalParams =
              { configSymbol, indexSymbol, configTokenName, indexTokenName }

          (createProposalTxHash /\ proposalSymbol /\ proposalTokenName) <-
            createProposal
              proposalParams
              sampleTallyStateDatum'

          void $ awaitTxConfirmedWithTimeout (Seconds 600.0)
            createProposalTxHash

          let
            voteParams =
              { configSymbol: configSymbol
              , tallySymbol: proposalSymbol
              , configTokenName: configTokenName
              -- Vote NFT (voting pass) symbol and token name
              , voteNftSymbol: votePassSymbol
              , voteTokenName: adaToken -- votePassTokenName
              -- Fungible
              , fungibleSymbol: fungibleSymbol
              -- Vote datum fields
              , proposalTokenName: proposalTokenName
              , voteDirection: VoteDirection'For
              , returnAda: (BigInt.fromInt 0)
              }

          (voteOnProposalTxHash /\ voteOnProposalSymbol) <- voteOnProposal
            voteParams

          void $ awaitTxConfirmedWithTimeout (Seconds 600.0)
            voteOnProposalTxHash

          let
            countVoteParams =
              { voteNftSymbol: votePassSymbol
              , voteTokenName: adaToken
              , voteNftTokenName: votePassTokenName
              , configSymbol: configSymbol
              , configTokenName: configTokenName
              , tallySymbol: proposalSymbol
              , fungibleSymbol: fungibleSymbol
              , fungibleTokenName: fungibleTokenName
              , fungiblePercent: BigInt.fromInt 10
              }

          collateral <- getWalletCollateral
          logInfo' $ "collateral: " <> show collateral

          countVoteTxHash <- countVote countVoteParams

          void $ awaitTxConfirmedWithTimeout (Seconds 600.0)
            countVoteTxHash

          pure unit

{-|
Module: Test.Workflow.CancelVote
Description: Test the cancel vote workflow
-}
module Test.Workflow.CancelVote (suite) where

import Contract.Prelude (Unit, bind, discard, pure, unit, void, ($), (/\))
import Contract.Test.Mote (TestPlanM)
import Contract.Test.Plutip
  ( InitialUTxOs
  , PlutipTest
  , withKeyWallet
  , withWallets
  )
import Contract.Transaction (awaitTxConfirmedWithTimeout)
import Contract.Value (adaSymbol, adaToken, scriptCurrencySymbol)
import Dao.Workflow.CancelVote (cancelVote)
import Dao.Workflow.CreateConfig (createConfig)
import Dao.Workflow.CreateIndex (createIndex)
import Dao.Workflow.CreateProposal (createProposal)
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
    test "Create proposal test" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 5_000_000
          , BigInt.fromInt 2_000_000_000
          ]
      withWallets distribution \wallet -> do
        withKeyWallet wallet do
          (createConfigTxHash /\ configSymbol /\ configTokenName) <-
            createConfig sampleConfigParams
          void $ awaitTxConfirmedWithTimeout (Seconds 600.0) createConfigTxHash

          (createIndexTxHash /\ indexSymbol /\ indexTokenName) <-
            createIndex adaToken
          void $ awaitTxConfirmedWithTimeout (Seconds 600.0) createIndexTxHash

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

          voteNftPolicy' <- voteNftPolicy
          let
            voteParams =
              { configSymbol: configSymbol
              , tallySymbol: proposalSymbol
              , configTokenName: configTokenName
              , voteTokenName: adaToken
              , voteNftSymbol: scriptCurrencySymbol voteNftPolicy'
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
            cancelVoteParams =
              { configSymbol: configSymbol
              , configTokenName: configTokenName
              , voteTokenName: adaToken
              }
          cancelVoteTxHash <- cancelVote cancelVoteParams

          void $ awaitTxConfirmedWithTimeout (Seconds 600.0) cancelVoteTxHash

          pure unit

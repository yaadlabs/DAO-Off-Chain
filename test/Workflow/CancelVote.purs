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
import Dao.Component.Proposal.Params
  ( CreateProposalParams(CreateProposalParams)
  )
import Dao.Component.Vote.Params
  ( CancelVoteParams(CancelVoteParams)
  , VoteOnProposalParams(VoteOnProposalParams)
  )
import Dao.Scripts.Policy.VoteNft (voteNftPolicy)
import Dao.Utils.Contract (ContractResult(ContractResult))
import Dao.Workflow.CancelVote (cancelVote)
import Dao.Workflow.CreateConfig (createConfig)
import Dao.Workflow.CreateIndex (createIndex)
import Dao.Workflow.CreateProposal (createProposal)
import Dao.Workflow.VoteOnProposal (VoteOnProposalResult(VoteOnProposalResult), voteOnProposal)
import Data.Time.Duration (Seconds(Seconds))
import JS.BigInt (fromInt) as BigInt
import LambdaBuffers.ApplicationTypes.Vote (VoteDirection(VoteDirection'For))
import Mote (group, test)
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
          ContractResult
            { txHash: configTxHash
            , symbol: configSymbol
            , tokenName: configTokenName
            } <-
            createConfig sampleConfigParams
          void $ awaitTxConfirmedWithTimeout (Seconds 600.0) configTxHash

          ContractResult
            { txHash: indexTxHash
            , symbol: indexSymbol
            , tokenName: indexTokenName
            } <-
            createIndex adaToken
          void $ awaitTxConfirmedWithTimeout (Seconds 600.0) indexTxHash

          tallyStateDatum <- sampleTallyStateDatum

          let
            proposalParams = CreateProposalParams
              { configSymbol
              , indexSymbol
              , configTokenName
              , indexTokenName
              , tallyStateDatum
              }

          ContractResult
            { txHash: createProposalTxHash
            , symbol: proposalSymbol
            , tokenName: proposalTokenName
            } <-
            createProposal proposalParams

          void $ awaitTxConfirmedWithTimeout (Seconds 600.0)
            createProposalTxHash

          voteNftPolicy' <- voteNftPolicy
          let
            voteParams = VoteOnProposalParams
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

          VoteOnProposalResult { txHash: voteOnProposalTxHash, voteSymbol } <-
            voteOnProposal
              voteParams

          void $ awaitTxConfirmedWithTimeout (Seconds 600.0)
            voteOnProposalTxHash

          let
            cancelVoteParams = CancelVoteParams
              { configSymbol: configSymbol
              , configTokenName: configTokenName
              , voteTokenName: adaToken
              }
          cancelVoteTxHash <- cancelVote cancelVoteParams

          void $ awaitTxConfirmedWithTimeout (Seconds 600.0) cancelVoteTxHash

          pure unit

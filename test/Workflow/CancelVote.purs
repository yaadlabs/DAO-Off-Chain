{-|
Module: Test.Workflow.CancelVote
Description: Test the cancel vote workflow
-}
module Test.Workflow.CancelVote (suite) where

import Contract.Address (PaymentPubKeyHash)
import Contract.Monad (liftedM)
import Contract.Prelude
  ( type (/\)
  , Unit
  , bind
  , discard
  , pure
  , void
  , ($)
  , (/\)
  )
import Contract.Test.Mote (TestPlanM)
import Contract.Test.Plutip
  ( InitialUTxOs
  , PlutipTest
  , withKeyWallet
  , withWallets
  )
import Contract.Transaction (awaitTxConfirmedWithTimeout)
import Contract.Value (adaSymbol, adaToken)
import Contract.Wallet (getWalletAddress, ownPaymentPubKeyHash)
import Dao.Component.Config.Params (ConfigParams)
import Dao.Workflow.CancelVote (cancelVote)
import Dao.Workflow.CreateConfig (createConfig)
import Dao.Workflow.CreateFungible (createFungible)
import Dao.Workflow.CreateIndex (createIndex)
import Dao.Workflow.CreateProposal (createProposal)
import Dao.Workflow.CreateVotePass (createVotePass)
import Dao.Workflow.VoteOnProposal (voteOnProposal)
import Data.Time.Duration (Seconds(Seconds))
import JS.BigInt (BigInt)
import JS.BigInt (fromInt) as BigInt
import LambdaBuffers.ApplicationTypes.Vote (VoteDirection(VoteDirection'For))
import Mote (group, test)
import Test.Data.Tally (sampleGeneralProposalTallyStateDatum)

suite :: TestPlanM PlutipTest Unit
suite = do
  group "DAO tests" do
    test "Cancel vote on proposal test" do
      let
        distribution :: (Array BigInt /\ Array BigInt)
        distribution =
          [ BigInt.fromInt 2_000_000_000
          , BigInt.fromInt 500_000_000
          ] /\ [ BigInt.fromInt 2_000_000_000 ]
      withWallets distribution \(walletOne /\ walletTwo) -> do
        walletTwoAddress <- withKeyWallet walletTwo do
          liftedM "Could not get wallet address" getWalletAddress

        withKeyWallet walletOne do
          userPkh :: PaymentPubKeyHash <- liftedM "Could not get pkh"
            ownPaymentPubKeyHash

          (votePassTxHash /\ votePassSymbol /\ votePassTokenName) <-
            createVotePass userPkh
          void $ awaitTxConfirmedWithTimeout (Seconds 600.0) votePassTxHash

          (fungibleTxHash /\ fungibleSymbol /\ fungibleTokenName) <-
            createFungible userPkh (BigInt.fromInt 2)
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
              , voteFungibleCurrencySymbol: adaSymbol
              , voteFungibleTokenName: adaToken
              , fungibleVotePercent: BigInt.fromInt 0
              -- Index needed for making tallyNft
              , indexSymbol: indexSymbol
              , indexTokenName: indexTokenName
              }

          (createConfigTxHash /\ configSymbol /\ configTokenName) <-
            createConfig sampleConfigParams
          void $ awaitTxConfirmedWithTimeout (Seconds 600.0) createConfigTxHash

          let
            sampleTallyStateDatum' = sampleGeneralProposalTallyStateDatum
              walletTwoAddress

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
            cancelVoteParams =
              { configSymbol: configSymbol
              , configTokenName: configTokenName
              , voteTokenName: adaToken
              , voteNftSymbol: votePassSymbol
              , voteNftTokenName: votePassTokenName
              , fungibleSymbol: fungibleSymbol
              , fungibleTokenName: fungibleTokenName
              }
          cancelVoteTxHash <- cancelVote cancelVoteParams

          void $ awaitTxConfirmedWithTimeout (Seconds 600.0) cancelVoteTxHash

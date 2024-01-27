{-|
Module: Test.Workflow.CancelVote
Description: Test the cancel vote workflow
-}
module Test.Workflow.CancelVote (suite) where

import Contract.Address (Address, PaymentPubKeyHash)
import Contract.Monad (liftContractM, liftedM)
import Contract.Prelude
  ( type (/\)
  , Unit
  , bind
  , discard
  , pure
  , unit
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
import Contract.Value (TokenName, adaSymbol, adaToken, scriptCurrencySymbol)
import Contract.Wallet (getWalletAddress, ownPaymentPubKeyHash)
import Dao.Component.Config.Params (CreateConfigParams(CreateConfigParams))
import Dao.Component.Fungible.Params
  ( CreateFungibleParams(CreateFungibleParams)
  )
import Dao.Component.Proposal.Params
  ( CreateProposalParams(CreateProposalParams)
  )
import Dao.Component.Vote.Params
  ( CancelVoteParams(CancelVoteParams)
  , VoteOnProposalParams(VoteOnProposalParams)
  )
import Dao.Scripts.Policy.Fungible (fungiblePolicy)
import Dao.Scripts.Policy.VoteNft (voteNftPolicy)
import Dao.Utils.Contract (ContractResult(ContractResult))
import Dao.Utils.Value (mkTokenName)
import Dao.Workflow.CancelVote (cancelVote)
import Dao.Workflow.CreateConfig (createConfig)
import Dao.Workflow.CreateFungible (createFungible)
import Dao.Workflow.CreateIndex (createIndex)
import Dao.Workflow.CreateProposal (createProposal)
import Dao.Workflow.CreateVotePass (createVotePass)
import Dao.Workflow.VoteOnProposal
  ( VoteOnProposalResult(VoteOnProposalResult)
  , voteOnProposal
  )
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
          userWalletAddress :: Address <- liftedM "Could not get wallet address"
            getWalletAddress
          pure userWalletAddress

        withKeyWallet walletOne do
          userPkh :: PaymentPubKeyHash <- liftedM "Could not get pkh"
            ownPaymentPubKeyHash
          userWalletAddress :: Address <- liftedM "Could not get wallet address"
            getWalletAddress

          ContractResult
            { txHash: votePassTxHash
            , symbol: votePassSymbol
            , tokenName: votePassTokenName
            } <-
            createVotePass userPkh
          void $ awaitTxConfirmedWithTimeout (Seconds 600.0) votePassTxHash

          let
            fungibleParams :: CreateFungibleParams
            fungibleParams = CreateFungibleParams
              { userPkh, amount: BigInt.fromInt 400 }

          ContractResult
            { txHash: fungibleTxHash
            , symbol: fungibleSymbol
            , tokenName: fungibleTokenName
            } <- createFungible fungibleParams
          void $ awaitTxConfirmedWithTimeout (Seconds 600.0) fungibleTxHash

          ContractResult
            { txHash: indexTxHash
            , symbol: indexSymbol
            , tokenName: indexTokenName
            } <-
            createIndex adaToken
          void $ awaitTxConfirmedWithTimeout (Seconds 600.0) indexTxHash

          -- The policy for the 'voteNft' token (vote pass)
          votePassPolicy <- voteNftPolicy

          -- The policy for the 'fungible' token (vote multiplier)
          fungiblePolicy' <- fungiblePolicy

          -- The fungible token name is hardcoded to this for now
          fungibleTokenName :: TokenName <-
            liftContractM "Could not make voteNft token name" $ mkTokenName
              "vote_fungible"
          let
            votePassSymbol = scriptCurrencySymbol votePassPolicy
            fungibleSymbol = scriptCurrencySymbol fungiblePolicy'

            sampleConfigParams :: CreateConfigParams
            sampleConfigParams = CreateConfigParams
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
              , fungibleVotePercent: BigInt.fromInt 0
              , voteNftSymbol: votePassSymbol
              -- Index needed for making tallyNft
              , indexSymbol: indexSymbol
              , indexTokenName: indexTokenName
              }

          ContractResult
            { txHash: createConfigTxHash
            , symbol: configSymbol
            , tokenName: configTokenName
            } <-
            createConfig sampleConfigParams
          void $ awaitTxConfirmedWithTimeout (Seconds 600.0) createConfigTxHash

          let
            tallyStateDatum = sampleGeneralProposalTallyStateDatum
              walletTwoAddress

            proposalParams :: CreateProposalParams
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

          let
            voteParams :: VoteOnProposalParams
            voteParams = VoteOnProposalParams
              { configSymbol: configSymbol
              , configTokenName: configTokenName
              , tallySymbol: proposalSymbol
              , voteTokenName: adaToken
              -- Vote datum fields
              , proposalTokenName: proposalTokenName
              , voteDirection: VoteDirection'For
              , returnAda: (BigInt.fromInt 0)
              }

          VoteOnProposalResult
            { txHash: voteOnProposalTxHash, symbol: voteSymbol } <-
            voteOnProposal
              voteParams

          void $ awaitTxConfirmedWithTimeout (Seconds 600.0)
            voteOnProposalTxHash

          let
            cancelVoteParams :: CancelVoteParams
            cancelVoteParams = CancelVoteParams
              { configSymbol: configSymbol
              , configTokenName: configTokenName
              , voteTokenName: adaToken
              }
          cancelVoteTxHash <- cancelVote cancelVoteParams

          void $ awaitTxConfirmedWithTimeout (Seconds 600.0) cancelVoteTxHash

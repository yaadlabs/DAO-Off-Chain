{-|
Module: Test.Workflow.CountVote
Description: Test the count vote workflow
-}
module Test.Workflow.CountVote (suite) where

import Contract.Address (PaymentPubKeyHash)
import Contract.Log (logInfo')
import Contract.Monad (liftedM)
import Contract.Prelude
  ( type (/\)
  , Unit
  , bind
  , discard
  , pure
  , show
  , show
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
import Contract.Value (adaSymbol, adaToken)
import Contract.Wallet (getWalletAddress, ownPaymentPubKeyHash)
import Dao.Component.Config.Params (CreateConfigParams(CreateConfigParams))
import Dao.Component.Fungible.Params
  ( CreateFungibleParams(CreateFungibleParams)
  )
import Dao.Component.Proposal.Params
  ( CreateProposalParams(CreateProposalParams)
  )
import Dao.Component.Vote.Params
  ( CountVoteParams(CountVoteParams)
  , VoteOnProposalParams(VoteOnProposalParams)
  )
import Dao.Utils.Contract (ContractResult(ContractResult))
import Dao.Workflow.CountVote (countVote)
import Dao.Workflow.CreateConfig (createConfig)
import Dao.Workflow.CreateFungible (createFungible)
import Dao.Workflow.CreateIndex (createIndex)
import Dao.Workflow.CreateProposal (createProposal)
import Dao.Workflow.CreateVotePass (createVotePass)
import Dao.Workflow.VoteOnProposal
  ( VoteOnProposalResult(VoteOnProposalResult)
  , voteOnProposal
  )
import Data.Time.Duration (Seconds(Seconds))
import JS.BigInt (BigInt)
import JS.BigInt (fromInt) as BigInt
import LambdaBuffers.ApplicationTypes.Vote (VoteDirection(VoteDirection'For))
import Mote (group, test)
import Test.Data.Tally (sampleGeneralProposalTallyStateDatum)

suite :: TestPlanM PlutipTest Unit
suite = do
  group "DAO tests" do
    test "Count votes on proposal test" do
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
            { txHash: createIndexTxHash
            , symbol: indexSymbol
            , tokenName: indexTokenName
            } <-
            createIndex adaToken
          void $ awaitTxConfirmedWithTimeout (Seconds 600.0) createIndexTxHash

          let
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
              , fungibleVotePercent: BigInt.fromInt 10

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

          VoteOnProposalResult
            { txHash: voteOnProposalTxHash
            , symbol: voteOnProposalSymbol
            } <- voteOnProposal voteParams

          void $ awaitTxConfirmedWithTimeout (Seconds 600.0)
            voteOnProposalTxHash

          let
            countVoteParams :: CountVoteParams
            countVoteParams = CountVoteParams
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

          countVoteTxHash <- countVote countVoteParams

          void $ awaitTxConfirmedWithTimeout (Seconds 600.0)
            countVoteTxHash

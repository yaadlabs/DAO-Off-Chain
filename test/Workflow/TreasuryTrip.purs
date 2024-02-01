{-|
Module: Test.Workflow.TreasuryTrip
Description: Test the treasury trip workflow
-}
module Test.Workflow.TreasuryTrip (suite) where

import Contract.Address (Address, PaymentPubKeyHash)
import Contract.Chain (waitNSlots)
import Contract.Log (logInfo')
import Contract.Monad (liftContractM, liftedM)
import Contract.Numeric.Natural (fromInt') as Natural
import Contract.Prelude
  ( type (/\)
  , Unit
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
import Contract.Value
  ( TokenName
  , adaSymbol
  , adaToken
  , scriptCurrencySymbol
  )
import Contract.Wallet
  ( getWalletAddress
  , getWalletCollateral
  , ownPaymentPubKeyHash
  )
import Dao.Component.Config.Params (CreateConfigParams(CreateConfigParams))
import Dao.Component.Fungible.Params
  ( CreateFungibleParams(CreateFungibleParams)
  )
import Dao.Component.Proposal.Params
  ( CreateProposalParams(CreateProposalParams)
  )
import Dao.Component.Treasury.Params (TreasuryParams(TreasuryParams))
import Dao.Component.Vote.Params
  ( CountVoteParams(CountVoteParams)
  , VoteOnProposalParams(VoteOnProposalParams)
  )
import Dao.Scripts.Policy.Fungible (fungiblePolicy)
import Dao.Scripts.Policy.VoteNft (voteNftPolicy)
import Dao.Utils.Contract (ContractResult(ContractResult))
import Dao.Utils.Value (mkTokenName)
import Dao.Workflow.CountVote (countVote)
import Dao.Workflow.CreateConfig (createConfig)
import Dao.Workflow.CreateFungible (createFungible)
import Dao.Workflow.CreateIndex (createIndex)
import Dao.Workflow.CreateProposal (createProposal)
import Dao.Workflow.CreateTreasuryFund (createTreasuryFund)
import Dao.Workflow.CreateVotePass (createVotePass)
import Dao.Workflow.TreasuryTrip (treasuryTrip)
import Dao.Workflow.VoteOnProposal
  ( VoteOnProposalResult(VoteOnProposalResult)
  , voteOnProposal
  )
import Data.Time.Duration (Seconds(Seconds))
import JS.BigInt (BigInt)
import JS.BigInt (fromInt) as BigInt
import LambdaBuffers.ApplicationTypes.Vote (VoteDirection(VoteDirection'For))
import Mote (group, test)
import Test.Data.Address (dummyAddress)
import Test.Data.Tally (sampleTripProposalTallyStateDatum)

suite :: TestPlanM PlutipTest Unit
suite = do
  group "DAO tests" do
    test "Treasury trip test" do
      let
        distribution :: (Array BigInt /\ Array BigInt /\ Array BigInt)
        distribution =
          [ BigInt.fromInt 2_000_000_000
          , BigInt.fromInt 500_000_000
          ] /\ [ BigInt.fromInt 2_000_000_000 ]
            /\ [ BigInt.fromInt 2_000_000_000 ]

      withWallets distribution \(walletOne /\ walletTwo /\ walletThree) -> do

        walletTwoAddress <- withKeyWallet walletTwo do
          liftedM "Could not get wallet address" getWalletAddress

        walletThreeAddress <- withKeyWallet walletThree do
          liftedM "Could not get wallet address" getWalletAddress

        withKeyWallet walletOne do

          userPkh :: PaymentPubKeyHash <- liftedM "Could not get pkh"
            ownPaymentPubKeyHash
          userWalletAddress :: Address <- liftedM "Could not get wallet address"
            getWalletAddress

          ContractResult
            { txHash: votePassTxHash
            , symbol: votePassSymbol
            , tokenName: votePassTokenName
            } <- createVotePass userPkh

          void $ awaitTxConfirmedWithTimeout (Seconds 600.0) votePassTxHash
          void $ waitNSlots (Natural.fromInt' 3)

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
            } <- createIndex adaToken
          void $ awaitTxConfirmedWithTimeout (Seconds 600.0) createIndexTxHash

          -- The policy for the 'voteNft' token (vote pass)
          votePassPolicy <- voteNftPolicy

          -- The policy for the 'fungible' token (vote multiplier)
          fungiblePolicy' <- fungiblePolicy

          -- The fungible token name is hardcoded to this for now
          fungibleTokenName :: TokenName <-
            liftContractM "Could not make voteNft token name" $ mkTokenName
              "vote_fungible"
          let
            -- The symbol for the 'voteNft' 
            -- This is the vote 'pass' that a user must possess
            -- in order to vote on a proposal
            votePassSymbol = scriptCurrencySymbol votePassPolicy

            -- The symbol for the 'fungibleSymbol'
            -- This acts as a vote multiplier for the user
            -- Without it the user's vote counts strictly for one (for or against)
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
              , totalVotes: BigInt.fromInt 1
              , maxGeneralDisbursement: BigInt.fromInt 200_000_000
              , maxTripDisbursement: BigInt.fromInt 20_000_000
              , agentDisbursementPercent: BigInt.fromInt 1
              , proposalTallyEndOffset: BigInt.fromInt 0
              , voteTokenName: adaToken
              , voteFungibleCurrencySymbol: fungibleSymbol
              , voteFungibleTokenName: fungibleTokenName
              , voteNftSymbol: votePassSymbol
              , fungibleVotePercent: BigInt.fromInt 10
              -- Index needed for making tallyNft
              , indexSymbol: indexSymbol
              , indexTokenName: indexTokenName
              }

          ContractResult
            { txHash: createConfigTxHash
            , symbol: configSymbol
            , tokenName: configTokenName
            } <- createConfig sampleConfigParams

          void $ awaitTxConfirmedWithTimeout (Seconds 600.0) createConfigTxHash
          void $ waitNSlots (Natural.fromInt' 3)

          let
            treasuryFundParams =
              { adaAmount: BigInt.fromInt 200_000_000
              , configSymbol: configSymbol
              , configTokenName: configTokenName
              }

          (treasuryFundTxHash /\ treasuryFundSymbol) <- createTreasuryFund
            treasuryFundParams

          void $ awaitTxConfirmedWithTimeout (Seconds 600.0) treasuryFundTxHash
          void $ waitNSlots (Natural.fromInt' 3)

          let
            tallyStateDatum = sampleTripProposalTallyStateDatum
              walletTwoAddress
              walletThreeAddress

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
            } <- createProposal proposalParams

          void $ awaitTxConfirmedWithTimeout (Seconds 600.0)
            createProposalTxHash
          void $ waitNSlots (Natural.fromInt' 3)

          let
            voteParams :: VoteOnProposalParams
            voteParams = VoteOnProposalParams
              { configSymbol: configSymbol
              , tallySymbol: proposalSymbol
              , configTokenName: configTokenName
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
          void $ waitNSlots (Natural.fromInt' 3)

          let
            countVoteParams :: CountVoteParams
            countVoteParams = CountVoteParams
              { voteTokenName: adaToken
              , configSymbol
              , configTokenName
              , tallySymbol: proposalSymbol
              , proposalTokenName
              }

          countVoteTxHash <- countVote countVoteParams

          void $ awaitTxConfirmedWithTimeout (Seconds 600.0)
            countVoteTxHash
          void $ waitNSlots (Natural.fromInt' 3)

          let
            treasuryTripParams :: TreasuryParams
            treasuryTripParams = TreasuryParams
              { configSymbol: configSymbol
              , configTokenName: configTokenName
              , tallySymbol: proposalSymbol
              , treasurySymbol: treasuryFundSymbol
              }

          treasuryTxHash <- treasuryTrip treasuryTripParams

          void $ awaitTxConfirmedWithTimeout (Seconds 600.0) treasuryTxHash
          void $ waitNSlots (Natural.fromInt' 3)

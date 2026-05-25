{-|
Module: Test.Workflow.TreasuryTrip
Description: Test the treasury trip workflow
-}
module Test.Workflow.TreasuryTrip (suite) where

import Cardano.Plutus.Types.Address (fromCardano) as Plutus.Address
import Cardano.Plutus.Types.TokenName (adaToken)
import Cardano.Types (AssetName, BigNum)
import Cardano.Types.BigNum (fromInt) as BigNum
import Cardano.Types.PlutusScript (hash) as PlutusScript
import Contract.Address (Address, PaymentPubKeyHash)
import Contract.Chain (waitNSlots)
import Contract.Monad (liftContractM, liftedM)
import Contract.Prelude
  ( type (/\)
  , Unit
  , bind
  , discard
  , void
  , ($)
  , (/\)
  )
import Contract.Test (ContractTest, withKeyWallet, withWallets)
import Contract.Test.Mote (TestPlanM)
import Contract.Transaction (awaitTxConfirmedWithTimeout)
import Contract.Wallet
  ( getWalletAddress
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
import Dao.Scripts.Policy (fungiblePolicy, voteNftPolicy)
import Dao.Utils.Contract (ContractResult(ContractResult))
import Dao.Utils.Value (mkTokenName)
import Dao.Workflow.CountVote (countVote)
import Dao.Workflow.CreateConfig
  ( CreateConfigResult(CreateConfigResult)
  , createConfig
  )
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
import Data.Newtype (unwrap)
import Data.Time.Duration (Seconds(Seconds))
import JS.BigInt (fromInt) as BigInt
import LambdaBuffers.ApplicationTypes.Vote (VoteDirection(VoteDirection'For))
import Mote (group, test)
import Test.Data.Tally (sampleTripProposalTallyStateDatum)

suite :: TestPlanM ContractTest Unit
suite = do
  group "DAO tests" do
    test "Treasury trip test" do
      let
        distribution :: (Array BigNum /\ Array BigNum /\ Array BigNum)
        distribution =
          [ BigNum.fromInt 2_000_000_000
          , BigNum.fromInt 500_000_000
          ] /\ [ BigNum.fromInt 2_000_000_000 ]
            /\ [ BigNum.fromInt 2_000_000_000 ]

      withWallets distribution \(walletOne /\ walletTwo /\ walletThree) -> do

        walletTwoAddress <-
          withKeyWallet walletTwo do
            addr <- liftedM "Could not get wallet address" getWalletAddress
            liftContractM "Could not convert walletTwoAddress" $
              Plutus.Address.fromCardano addr

        walletThreeAddress <-
          withKeyWallet walletThree do
            addr <- liftedM "Could not get wallet address" getWalletAddress
            liftContractM "Could not convert walletThreeAddress" $
              Plutus.Address.fromCardano addr

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
          void $ waitNSlots (BigNum.fromInt 3)

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
            } <- createIndex $ unwrap adaToken
          void $ awaitTxConfirmedWithTimeout (Seconds 600.0) createIndexTxHash

          -- The policy for the 'voteNft' token (vote pass)
          votePassPolicy <- voteNftPolicy

          -- The policy for the 'fungible' token (vote multiplier)
          fungiblePolicy' <- fungiblePolicy

          -- The fungible token name is hardcoded to this for now
          fungibleTokenName :: AssetName <-
            liftContractM "Could not make voteNft token name" $ mkTokenName
              "vote_fungible"
          let
            -- The symbol for the 'voteNft' 
            -- This is the vote 'pass' that a user must possess
            -- in order to vote on a proposal
            votePassSymbol = PlutusScript.hash votePassPolicy

            -- The symbol for the 'fungibleSymbol'
            -- This acts as a vote multiplier for the user
            -- Without it the user's vote counts strictly for one (for or against)
            fungibleSymbol = PlutusScript.hash fungiblePolicy'

            sampleConfigParams :: CreateConfigParams
            sampleConfigParams = CreateConfigParams
              { configTokenName: unwrap adaToken
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
              , voteTokenName: unwrap adaToken
              , voteFungibleCurrencySymbol: fungibleSymbol
              , voteFungibleTokenName: fungibleTokenName
              , voteNftSymbol: votePassSymbol
              , fungibleVotePercent: BigInt.fromInt 10
              -- Index needed for making tallyNft
              , indexSymbol: indexSymbol
              , indexTokenName: indexTokenName
              }

          CreateConfigResult
            { txHash: createConfigTxHash
            , configSymbol
            , configTokenName
            } <- createConfig sampleConfigParams

          void $ awaitTxConfirmedWithTimeout (Seconds 600.0) createConfigTxHash
          void $ waitNSlots (BigNum.fromInt 3)

          let
            treasuryFundParams =
              { adaAmount: BigInt.fromInt 200_000_000
              , configSymbol: configSymbol
              , configTokenName: configTokenName
              }

          ContractResult
            { txHash: treasuryFundTxHash
            , symbol: treasuryFundSymbol
            } <- createTreasuryFund treasuryFundParams

          void $ awaitTxConfirmedWithTimeout (Seconds 600.0) treasuryFundTxHash
          void $ waitNSlots (BigNum.fromInt 3)

          tallyStateDatum <- sampleTripProposalTallyStateDatum
            walletTwoAddress
            walletThreeAddress

          let
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
          void $ waitNSlots (BigNum.fromInt 3)

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
          void $ waitNSlots (BigNum.fromInt 3)

          let
            countVoteParams :: CountVoteParams
            countVoteParams = CountVoteParams
              { configSymbol
              , configTokenName
              , tallySymbol: proposalSymbol
              , proposalTokenName
              }

          countVoteTxHash <- countVote countVoteParams

          void $ awaitTxConfirmedWithTimeout (Seconds 600.0)
            countVoteTxHash
          void $ waitNSlots (BigNum.fromInt 3)

          let
            treasuryTripParams :: TreasuryParams
            treasuryTripParams = TreasuryParams
              { configSymbol
              , configTokenName
              , tallySymbol: proposalSymbol
              , proposalTokenName
              , treasurySymbol: treasuryFundSymbol
              }

          treasuryTxHash <- treasuryTrip treasuryTripParams

          void $ awaitTxConfirmedWithTimeout (Seconds 600.0) treasuryTxHash
          void $ waitNSlots (BigNum.fromInt 3)

{-|
Module: Test.Workflow.UpgradeConfig
Description: Test the upgrade config workflow
-}
module Test.Workflow.UpgradeConfig (suite) where

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
import Contract.Value (adaSymbol, adaToken, scriptCurrencySymbol)
import Contract.Wallet
  ( getWalletAddress
  , getWalletCollateral
  , ownPaymentPubKeyHash
  )
import Dao.Component.Config.Params (ConfigParams)
import Dao.Workflow.CountVote (countVote)
import Dao.Workflow.CreateConfig (createConfig)
import Dao.Workflow.CreateFungible (createFungible)
import Dao.Workflow.CreateIndex (createIndex)
import Dao.Workflow.CreateProposal (createProposal)
import Dao.Workflow.CreateTreasuryFund (createTreasuryFund)
import Dao.Workflow.CreateVotePass (createVotePass)
import Dao.Workflow.UpgradeConfig (upgradeConfig)
import Dao.Workflow.VoteOnProposal (voteOnProposal)
import Data.Time.Duration (Seconds(Seconds))
import JS.BigInt (BigInt)
import JS.BigInt (fromInt) as BigInt
import LambdaBuffers.ApplicationTypes.Vote (VoteDirection(VoteDirection'For))
import Mote (group, test)
import Scripts.UpgradePolicy (upgradePolicy)
import Test.Data.Address (dummyAddress)
import Test.Data.Config (dummyNewConfig)
import Test.Data.Tally (sampleUpgradeConfigProposalTallyStateDatum)

suite :: TestPlanM PlutipTest Unit
suite = do
  group "DAO tests" do
    test "Upgrade config test" do
      let
        distribution :: (Array BigInt)
        distribution =
          [ BigInt.fromInt 2_000_000_000
          , BigInt.fromInt 500_000_000
          ]

      withWallets distribution \walletOne -> do

        withKeyWallet walletOne do

          userPkh :: PaymentPubKeyHash <- liftedM "Could not get pkh"
            ownPaymentPubKeyHash

          (votePassTxHash /\ votePassSymbol /\ votePassTokenName) <-
            createVotePass userPkh
          void $ awaitTxConfirmedWithTimeout (Seconds 600.0) votePassTxHash

          (fungibleTxHash /\ fungibleSymbol /\ fungibleTokenName) <-
            createFungible userPkh (BigInt.fromInt 400)
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
              , totalVotes: BigInt.fromInt 1
              , maxGeneralDisbursement: BigInt.fromInt 200_000_000
              , maxTripDisbursement: BigInt.fromInt 20_000_000
              , agentDisbursementPercent: BigInt.fromInt 1
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

          let
            treasuryFundParams =
              { adaAmount: BigInt.fromInt 200_000_000
              , configSymbol: configSymbol
              , configTokenName: configTokenName
              }

          upgradePolicy' <- upgradePolicy
          let
            upgradePolicySymbol = scriptCurrencySymbol upgradePolicy'
            sampleTallyStateDatum' = sampleUpgradeConfigProposalTallyStateDatum
              upgradePolicySymbol

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
              , voteTokenName: adaToken
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

          countVoteTxHash <- countVote countVoteParams

          void $ awaitTxConfirmedWithTimeout (Seconds 600.0)
            countVoteTxHash

          dummyConfig <- dummyNewConfig
          let
            upgradeConfigParams =
              { configSymbol: configSymbol
              , configTokenName: configTokenName
              , tallySymbol: proposalSymbol
              , newDynamicConfigDatum: dummyConfig
              }

          treasuryTxHash <- upgradeConfig upgradeConfigParams

          void $ awaitTxConfirmedWithTimeout (Seconds 600.0) treasuryTxHash

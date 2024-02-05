{-|
Module: Test.Workflow.VoteOnProposal
Description: Test the vote on proposal workflow
-}
module Test.Workflow.VoteOnProposal (suite) where

import Contract.Address (PaymentPubKeyHash)
import Contract.Chain (waitNSlots)
import Contract.Monad (liftedM)
import Contract.Numeric.Natural (fromInt') as Natural
import Contract.Prelude (Unit, bind, discard, pure, unit, void, ($), (/\))
import Contract.Test.Mote (TestPlanM)
import Contract.Test.Plutip
  ( InitialUTxOs
  , PlutipTest
  , withKeyWallet
  , withWallets
  )
import Contract.Transaction (awaitTxConfirmedWithTimeout)
import Contract.Value (adaSymbol, adaToken)
import Contract.Wallet (ownPaymentPubKeyHash)
import Dao.Component.Config.Params (ConfigParams)
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
import Test.Data.Tally (sampleTallyStateDatum)

suite :: TestPlanM PlutipTest Unit
suite = do
  group "DAO tests" do
    test "Vote on proposal test" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 2_000_000_000
          , BigInt.fromInt 2_000_000_000
          ]
      withWallets distribution \wallet -> do
        withKeyWallet wallet do

          userPkh :: PaymentPubKeyHash <- liftedM "Could not get pkh"
            ownPaymentPubKeyHash

          (votePassTxHash /\ votePassSymbol /\ votePassTokenName) <-
            createVotePass userPkh

          void $ awaitTxConfirmedWithTimeout (Seconds 600.0) votePassTxHash
          void $ waitNSlots (Natural.fromInt' 2)

          (fungibleTxHash /\ fungibleSymbol /\ fungibleTokenName) <-
            createFungible userPkh (BigInt.fromInt 2)
          void $ awaitTxConfirmedWithTimeout (Seconds 600.0) fungibleTxHash

          (createIndexTxHash /\ indexSymbol /\ indexTokenName) <-
            createIndex adaToken
          void $ awaitTxConfirmedWithTimeout (Seconds 600.0) createIndexTxHash
          void $ waitNSlots (Natural.fromInt' 2)

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
              , voteFungibleCurrencySymbol: votePassSymbol -- adaSymbol
              , voteFungibleTokenName: adaToken
              , fungibleVotePercent: BigInt.fromInt 0

              -- Index needed for making tallyNft
              , indexSymbol
              , indexTokenName
              }

          (createConfigTxHash /\ configSymbol /\ configTokenName) <-
            createConfig sampleConfigParams
          void $ awaitTxConfirmedWithTimeout (Seconds 600.0) createConfigTxHash
          void $ waitNSlots (Natural.fromInt' 2)

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
          void $ waitNSlots (Natural.fromInt' 2)

          let
            voteParams =
              { configSymbol
              , tallySymbol: proposalSymbol
              , configTokenName: configTokenName
              -- Vote NFT (voting pass) symbol and token name
              , voteNftSymbol: votePassSymbol
              , voteTokenName: adaToken -- votePassTokenName
              -- Fungible
              , fungibleSymbol: fungibleSymbol
              -- Vote datum fields
              , proposalTokenName
              , voteDirection: VoteDirection'For
              , returnAda: (BigInt.fromInt 0)
              }

          (voteOnProposalTxHash /\ _voteOnProposalSymbol) <- voteOnProposal
            voteParams

          void $ awaitTxConfirmedWithTimeout (Seconds 600.0)
            voteOnProposalTxHash
          void $ waitNSlots (Natural.fromInt' 2)

          pure unit

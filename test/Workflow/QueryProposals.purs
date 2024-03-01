{-|
Module: Test.Workflow.QueryProposals
Description: Workflow for testing proposal querying functionality
-}
module Test.Workflow.QueryProposals (suite) where

import Contract.Address (Address, PaymentPubKeyHash)
import Contract.Chain (waitNSlots)
import Contract.Config (NetworkId(TestnetId))
import Contract.Log (logInfo')
import Contract.Monad (liftContractM, liftedM)
import Contract.Numeric.Natural (fromInt') as Natural
import Contract.Prelude
  ( type (/\)
  , Unit
  , bind
  , discard
  , mconcat
  , pure
  , show
  , show
  , unit
  , void
  , (#)
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
import Contract.Value (TokenName, adaSymbol, adaToken, scriptCurrencySymbol)
import Contract.Wallet (getWalletAddress, ownPaymentPubKeyHash)
import Dao.Component.Config.Params (CreateConfigParams(CreateConfigParams))
import Dao.Component.Fungible.Params
  ( CreateFungibleParams(CreateFungibleParams)
  )
import Dao.Component.Proposal.Params
  ( CreateProposalParams(CreateProposalParams)
  )
import Dao.Component.Treasury.Params (TreasuryParams(TreasuryParams))
import Dao.Component.Vote.Params
  ( CancelVoteParams(CancelVoteParams)
  , CountVoteParams(CountVoteParams)
  , VoteOnProposalParams(VoteOnProposalParams)
  )
import Dao.Scripts.Policy.Fungible (fungiblePolicy)
import Dao.Scripts.Policy.VoteNft (voteNftPolicy)
import Dao.Utils.Address (addressToPaymentPubKeyHash)
import Dao.Utils.Contract (ContractResult(ContractResult))
import Dao.Utils.Value (mkTokenName)
import Dao.Workflow.CancelVote (cancelVote)
import Dao.Workflow.CountVote (countVote)
import Dao.Workflow.CreateConfig (createConfig)
import Dao.Workflow.CreateFungible (createFungible)
import Dao.Workflow.CreateIndex (createIndex)
import Dao.Workflow.CreateProposal (createProposal)
import Dao.Workflow.CreateTreasuryFund (createTreasuryFund)
import Dao.Workflow.CreateVotePass (createVotePass)
import Dao.Workflow.TreasuryGeneral (treasuryGeneral)
import Dao.Workflow.VoteOnProposal
  ( VoteOnProposalResult(VoteOnProposalResult)
  , voteOnProposal
  )
import Data.Newtype (unwrap)
import Data.Time.Duration (Seconds(Seconds))
import JS.BigInt (BigInt)
import JS.BigInt (fromInt) as BigInt
import LambdaBuffers.ApplicationTypes.Vote (VoteDirection(VoteDirection'For))
import Mote (group, test)
import Test.Data.Tally (sampleGeneralProposalTallyStateDatum)

suite :: TestPlanM PlutipTest Unit
suite = do
  group "DAO tests" do
    test
      ( mconcat
          [ "Proposal queries: "
          , "--"
          ]
      )
      do
        let
          distribution ::
            ( Array BigInt
                /\ Array BigInt
                /\ Array BigInt
                /\ Array BigInt
                /\ Array BigInt
                /\ Array BigInt
            )
          distribution =
            [ BigInt.fromInt 2_000_000_000
            , BigInt.fromInt 500_000_000
            ] /\ [ BigInt.fromInt 2_000_000_000 ]
              /\ [ BigInt.fromInt 2_000_000_000 ]
              /\ [ BigInt.fromInt 2_000_000_000 ]
              /\ [ BigInt.fromInt 2_000_000_000 ]
              /\ [ BigInt.fromInt 2_000_000_000 ]
        withWallets distribution
          \( walletOne
               /\ walletTwo
               /\ walletThree
               /\ walletFour
               /\ walletFive
               /\ walletSix
           ) -> pure unit

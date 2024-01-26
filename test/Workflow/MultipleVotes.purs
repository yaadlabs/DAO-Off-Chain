{-|
Module: Test.Workflow.MultipleVotes
Description: Test the count vote workflow, where multiple votes have been cast
-}
module Test.Workflow.MultipleVotes (suite) where

import Contract.Address (Address, PaymentPubKeyHash)
import Contract.Config (NetworkId(TestnetId))
import Contract.Log (logInfo')
import Contract.Monad (liftContractM, liftedM)
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
import Contract.Value (adaSymbol, adaToken, scriptCurrencySymbol)
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
import Dao.Scripts.Policy.Fungible (fungiblePolicy)
import Dao.Scripts.Policy.VoteNft (voteNftPolicy)
import Dao.Utils.Address (addressToPaymentPubKeyHash)
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
    test "Count votes on proposal test" do
      let
        distribution ::
          ( Array BigInt
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
      withWallets distribution
        \(walletOne /\ walletTwo /\ walletThree /\ walletFour) -> do

          -- ******************************************************************************* --
          -- ******************************************************************************* --
          -- * Get the wallet address for user two to use in the create proposal           * --
          -- * section below. This will be used as the payment address for the tally datum * --
          userTwoWalletAddress :: Address <- withKeyWallet walletTwo do
            logInfo' "Running in walletTwo - first time"
            walletAddress :: Address <- liftedM "Could not get wallet address"
              getWalletAddress
            pure walletAddress

          -- ******************************************************************************* --
          -- ******************************************************************************* --
          -- * Get the wallet address for user three to use in the create proposal         * --
          -- * section below. This will be used as the payment address for the tally datum * --
          userThreeWalletAddress :: Address <- withKeyWallet walletThree do
            logInfo' "Running in wallet three - first time"
            walletAddress :: Address <- liftedM "Could not get wallet address"
              getWalletAddress
            pure walletAddress

          -- ******************************************************************************* --
          -- ******************************************************************************* --
          -- * Get the wallet address for user three to use in the create proposal         * --
          -- * section below. This will be used as the payment address for the tally datum * --
          userFourWalletAddress :: Address <- withKeyWallet walletFour do
            logInfo' "Running in wallet three - first time"
            walletAddress :: Address <- liftedM "Could not get wallet address"
              getWalletAddress
            pure walletAddress

          -- ************************************************************ --
          -- ************************************************************ --
          -- * User one creates a proposal on which the others can vote * --
          ( proposalSymbol /\ proposalTokenName /\ configSymbol /\
              configTokenName
          ) <- withKeyWallet walletOne do
            logInfo' "Running in walletOne - first time"

            -- Create the index UTXO with the index datum
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
            let
              -- The symbol for the 'voteNft' 
              -- This is the vote 'pass' that a user must possess
              -- in order to vote on a proposal
              votePassSymbol = scriptCurrencySymbol votePassPolicy

              -- The symbol for the 'fungibleSymbol'
              -- This acts as a vote multiplier for the user
              -- Without it the user's vote counts strictly for one (for or against)
              fungibleSymbol = scriptCurrencySymbol fungiblePolicy'

              -- The params needed for the initial dynamic config
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
                , voteFungibleTokenName: adaToken
                , voteNftSymbol: votePassSymbol
                , fungibleVotePercent: BigInt.fromInt 10
                -- Index needed for making tallyNft
                , indexSymbol: indexSymbol
                , indexTokenName: indexTokenName
                }

            -- Create the UTXO that holds the 'DynamicConfigDatum'
            ContractResult
              { txHash: createConfigTxHash
              , symbol: configSymbol
              , tokenName: configTokenName
              } <- createConfig sampleConfigParams
            void $ awaitTxConfirmedWithTimeout (Seconds 600.0)
              createConfigTxHash

            let
              -- The 'TallyStateDatum' to be sent to the proposal UTXO
              -- This proposal type for this proposal will be a 'General' one
              tallyStateDatum = sampleGeneralProposalTallyStateDatum
                userTwoWalletAddress

              -- The params needed for creating the proposal
              proposalParams :: CreateProposalParams
              proposalParams = CreateProposalParams
                { configSymbol
                , indexSymbol
                , configTokenName
                , indexTokenName
                , tallyStateDatum
                }

            -- Create the proposal UTXO
            ContractResult
              { txHash: createProposalTxHash
              , symbol: proposalSymbol
              , tokenName: proposalTokenName
              } <- createProposal proposalParams

            void $ awaitTxConfirmedWithTimeout (Seconds 600.0)
              createProposalTxHash

            -- Get the PKH for user three (walletThree)
            userThreePkh :: PaymentPubKeyHash <-
              liftContractM "Could not get pkh" $
                addressToPaymentPubKeyHash userThreeWalletAddress

            -- Create a vote pass for a user
            -- (third wallet as we use the second wallet as the paymentAddress in the tally datum)
            ContractResult
              { txHash: votePassTxHashUserThree
              , symbol: votePassSymbolUserThree
              , tokenName: votePassTokenNameUserThree
              } <- createVotePass userThreePkh

            void $ awaitTxConfirmedWithTimeout (Seconds 600.0)
              votePassTxHashUserThree

            -- Create the fungible token (amount 400) for user three (walletThree) 
            let
              fungibleParamsUserThree :: CreateFungibleParams
              fungibleParamsUserThree = CreateFungibleParams
                { userPkh: userThreePkh, amount: BigInt.fromInt 400 }

            ContractResult
              { txHash: fungibleTxHashUserThree
              , symbol: fungibleSymbolUserThree
              , tokenName: fungibleTokenNameUserThree
              } <- createFungible fungibleParamsUserThree
            void $ awaitTxConfirmedWithTimeout (Seconds 600.0)
              fungibleTxHashUserThree

            -- Get the PKH for user four (walletFour)
            userFourPkh :: PaymentPubKeyHash <-
              liftContractM "Could not get pkh" $
                addressToPaymentPubKeyHash userFourWalletAddress

            -- Create a vote pass for a second user (fourth wallet)
            ContractResult
              { txHash: votePassTxHashUserFour
              , symbol: votePassSymbolUserFour
              , tokenName: votePassTokenNameUserFour
              } <- createVotePass userFourPkh

            void $ awaitTxConfirmedWithTimeout (Seconds 600.0)
              votePassTxHashUserFour

            -- Create the fungible token (amount 100) for user four (walletFour) 
            let
              fungibleParamsUserFour :: CreateFungibleParams
              fungibleParamsUserFour = CreateFungibleParams
                { userPkh: userFourPkh, amount: BigInt.fromInt 100 }

            ContractResult
              { txHash: fungibleTxHashUserFour
              , symbol: fungibleSymbolUserFour
              , tokenName: fungibleTokenNameUserFour
              } <- createFungible fungibleParamsUserFour
            void $ awaitTxConfirmedWithTimeout (Seconds 600.0)
              fungibleTxHashUserFour

            pure
              ( proposalSymbol /\ proposalTokenName /\ configSymbol /\
                  configTokenName
              )

          -- ************************************************** --
          -- ************************************************** --
          -- * User three (walletThree) votes on the proposal * --
          withKeyWallet walletThree do
            logInfo'
              "Running in wallet three - voting on proposal created by walletOne"

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
              { txHash: voteOnProposalTxHash
              , symbol: voteOnProposalSymbol
              } <- voteOnProposal voteParams

            void $ awaitTxConfirmedWithTimeout (Seconds 600.0)
              voteOnProposalTxHash

            pure unit

          -- ************************************************** --
          -- ************************************************** --
          -- * User four (walletFour) votes on the proposal * --
          withKeyWallet walletFour do
            logInfo'
              "Running in wallet four - voting on proposal created by walletOne"

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
              { txHash: voteOnProposalTxHash
              , symbol: voteOnProposalSymbol
              } <- voteOnProposal voteParams

            void $ awaitTxConfirmedWithTimeout (Seconds 600.0)
              voteOnProposalTxHash

            pure unit

          -- ********************************************************* --
          -- ********************************************************* --
          -- * User one (walletOne) counts the votes on the proposal * --
          withKeyWallet walletFour do
            logInfo'
              "Running in wallet one - counting the votes on the proposal created by walletOne"

            let
              countVoteParams :: CountVoteParams
              countVoteParams = CountVoteParams
                { voteTokenName: adaToken
                , configSymbol: configSymbol
                , configTokenName: configTokenName
                , tallySymbol: proposalSymbol
                , fungiblePercent: BigInt.fromInt 10
                }

            countVoteTxHash <- countVote countVoteParams

            void $ awaitTxConfirmedWithTimeout (Seconds 600.0)
              countVoteTxHash

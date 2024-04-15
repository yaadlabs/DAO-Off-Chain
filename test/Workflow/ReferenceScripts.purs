{-|
Module: Test.Workflow.MultipleVotesWithCancel
Description: Workflow that includes multiple votes and one user cancelling their
  vote before votes are counted and the general treasury effect is executed.
-}
module Test.Workflow.ReferenceScripts (suite) where

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
import Dao.Component.Config.Params
  ( CreateConfigParams(CreateConfigParams)
  , mkValidatorConfig
  )
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
import Dao.Scripts.Policy (fungiblePolicy, voteNftPolicy)
import Dao.Utils.Address (addressToPaymentPubKeyHash)
import Dao.Utils.Contract (ContractResult(ContractResult))
import Dao.Utils.Value (mkTokenName)
import Dao.Workflow.CancelVote (cancelVote)
import Dao.Workflow.CountVote (countVote)
import Dao.Workflow.CreateConfig (CreateConfigResult(CreateConfigResult), createConfig)
import Dao.Workflow.CreateFungible (createFungible)
import Dao.Workflow.CreateIndex (createIndex)
import Dao.Workflow.CreateProposal (createProposal)
import Dao.Workflow.CreateTreasuryFund (createTreasuryFund)
import Dao.Workflow.CreateVotePass (createVotePass)
import Dao.Workflow.ReferenceScripts
  ( deployReferenceScriptsOne
  , deployReferenceScriptsThree
  , deployReferenceScriptsTwo
  )
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
          [ "Full workflow: "
          , "with multiple proposals, "
          , "multiple votes on the proposals, "
          , "a vote cancelled, "
          , "a vote cancelled but then cast again, "
          , "treasury effects executed"
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
           ) -> do

            -- ******************************************************************************* --
            -- ******************************************************************************* --
            -- * Get the wallet address for user two to use in the first create proposal     * --
            -- * section below. This will be used as the payment address for the tally datum * --
            userTwoWalletAddress :: Address <- withKeyWallet walletTwo do
              logInfo' "Running in walletTwo - first time"
              walletAddress :: Address <- liftedM "Could not get wallet address"
                getWalletAddress
              pure walletAddress

            -- ******************************************************************************* --
            -- ******************************************************************************* --
            -- * Get the wallet address for user six to use in the second create proposal    * --
            -- * section below. This will be used as the payment address for the tally datum * --
            userSixWalletAddress :: Address <- withKeyWallet walletSix do
              logInfo' "Running in walletSix - first time"
              walletAddress :: Address <- liftedM "Could not get wallet address"
                getWalletAddress
              pure walletAddress

            -- ************************************************************************ --
            -- ************************************************************************ --
            -- * Get the wallet address for user three to use in the create proposal  * --
            -- * section below. We need this in order to send a vote pass to the user * --
            -- * so they can cast a vote, and to send fungible tokens to them so they * --
            -- * can increase the value of their vote.                                * --
            userThreeWalletAddress :: Address <- withKeyWallet walletThree do
              logInfo' "Running in wallet three - first time"
              walletAddress :: Address <- liftedM "Could not get wallet address"
                getWalletAddress
              pure walletAddress

            -- ************************************************************************ --
            -- ************************************************************************ --
            -- * Get the wallet address for user four to use in the create proposal   * --
            -- * section below, for the same reasons stated above.                    * --
            userFourWalletAddress :: Address <- withKeyWallet walletFour do
              logInfo' "Running in wallet four - first time"
              walletAddress :: Address <- liftedM "Could not get wallet address"
                getWalletAddress
              pure walletAddress

            -- ************************************************************************ --
            -- ************************************************************************ --
            -- * Get the wallet address for user five to use in the create proposal   * --
            userFiveWalletAddress :: Address <- withKeyWallet walletFive do
              logInfo' "Running in wallet five - first time"
              walletAddress :: Address <- liftedM "Could not get wallet address"
                getWalletAddress
              pure walletAddress

            -- *************************************************************** --
            -- *************************************************************** --
            -- * User one creates the config and deploys the reference scripts * --
            (configSymbol /\ configTokenName /\ indexSymbol /\ indexTokenName) <-
              withKeyWallet walletOne do
                logInfo'
                  "Running in walletOne - walletOne creates config and deploys reference scripts"

                -- Create the index UTXO with the index datum
                ContractResult
                  { txHash: createIndexTxHash
                  , symbol: indexSymbol
                  , tokenName: indexTokenName
                  } <- createIndex adaToken

                void $ awaitTxConfirmedWithTimeout (Seconds 600.0)
                  createIndexTxHash
                void $ waitNSlots (Natural.fromInt' 3)

                -- The policy for the 'voteNft' token (vote pass)
                votePassPolicy <- voteNftPolicy

                -- The policy for the 'fungible' token (vote multiplier)
                fungiblePolicy' <- fungiblePolicy

                -- The fungible token name is hardcoded to this for now
                fungibleTokenName :: TokenName <-
                  liftContractM "Could not make voteNft token name" $
                    mkTokenName
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
                    , totalVotes: BigInt.fromInt 1
                    , maxGeneralDisbursement: BigInt.fromInt 200_000_000
                    , maxTripDisbursement: BigInt.fromInt 0
                    , agentDisbursementPercent: BigInt.fromInt 0
                    , proposalTallyEndOffset: BigInt.fromInt 0
                    , voteTokenName: adaToken
                    , voteFungibleCurrencySymbol: fungibleSymbol
                    , voteFungibleTokenName: fungibleTokenName
                    , voteNftSymbol: votePassSymbol
                    , fungibleVotePercent: BigInt.fromInt 10
                    -- Index needed for making tallyNft
                    , indexSymbol
                    , indexTokenName
                    }

                -- Create the UTXO that holds the 'DynamicConfigDatum'
                CreateConfigResult
                  { txHash: createConfigTxHash
                  , configSymbol
                  , configTokenName
                  } <- createConfig sampleConfigParams

                void $ awaitTxConfirmedWithTimeout (Seconds 600.0)
                  createConfigTxHash
                void $ waitNSlots (Natural.fromInt' 3)

                _ <- deployReferenceScriptsOne
                  (mkValidatorConfig configSymbol configTokenName)
                _ <- deployReferenceScriptsTwo
                  (mkValidatorConfig configSymbol configTokenName)
                _ <- deployReferenceScriptsThree
                  (mkValidatorConfig configSymbol configTokenName)

                pure
                  ( configSymbol /\ configTokenName /\ indexSymbol /\
                      indexTokenName
                  )

            (proposalOneSymbol /\ proposalOneTokenName) <- withKeyWallet
              walletOne
              do
                logInfo'
                  "Running in walletOne - walletOne creates the first proposal"

                tallyStateDatum <- sampleGeneralProposalTallyStateDatum
                  userTwoWalletAddress

                let
                  -- The params needed for creating the first proposal
                  proposalParams :: CreateProposalParams
                  proposalParams = CreateProposalParams
                    { configSymbol
                    , indexSymbol
                    , configTokenName
                    , indexTokenName
                    , tallyStateDatum
                    }

                -- Create the first proposal UTXO
                ContractResult
                  { txHash: createProposalTxHash
                  , symbol: proposalOneSymbol
                  , tokenName: proposalOneTokenName
                  } <- createProposal proposalParams

                void $ awaitTxConfirmedWithTimeout (Seconds 600.0)
                  createProposalTxHash
                void $ waitNSlots (Natural.fromInt' 3)

                pure (proposalOneSymbol /\ proposalOneTokenName)

            -- *************************************** --
            -- *************************************** --
            -- * User one creates the treasury fund  * --
            treasuryFundSymbol <- withKeyWallet walletOne do

              logInfo' "Running in wallet one - creating treasury fund"
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

              void $ awaitTxConfirmedWithTimeout (Seconds 600.0)
                treasuryFundTxHash
              void $ waitNSlots (Natural.fromInt' 3)

              pure treasuryFundSymbol

            _ <- withKeyWallet walletOne do

              -- **************************************************************************** --
              -- Create the voting credentials (vote pass and fungible tokens) for user three --
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
              void $ waitNSlots (Natural.fromInt' 3)

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
              void $ waitNSlots (Natural.fromInt' 3)

              -- ******************************************************************************* --
              -- * Create the voting credentials (vote pass and fungible tokens) for user four * --

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
              void $ waitNSlots (Natural.fromInt' 3)

              -- Create the fungible token (amount 200) for user four (walletFour)
              let
                fungibleParamsUserFour :: CreateFungibleParams
                fungibleParamsUserFour = CreateFungibleParams
                  { userPkh: userFourPkh, amount: BigInt.fromInt 200 }

              ContractResult
                { txHash: fungibleTxHashUserFour
                , symbol: fungibleSymbolUserFour
                , tokenName: fungibleTokenNameUserFour
                } <- createFungible fungibleParamsUserFour

              void $ awaitTxConfirmedWithTimeout (Seconds 600.0)
                fungibleTxHashUserFour
              void $ waitNSlots (Natural.fromInt' 3)

              -- ******************************************************************************* --
              -- * Create the voting credentials (vote pass and fungible tokens) for user five * --

              -- Get the PKH for user five (walletFive)
              userFivePkh :: PaymentPubKeyHash <-
                liftContractM "Could not get pkh" $
                  addressToPaymentPubKeyHash userFiveWalletAddress

              -- Create a vote pass for a third user (fifth wallet)
              ContractResult
                { txHash: votePassTxHashUserFive
                , symbol: votePassSymbolUserFive
                , tokenName: votePassTokenNameUserFive
                } <- createVotePass userFivePkh

              void $ awaitTxConfirmedWithTimeout (Seconds 600.0)
                votePassTxHashUserFive
              void $ waitNSlots (Natural.fromInt' 3)

              -- Create the fungible token (amount 600) for user five (walletFive)
              let
                fungibleParamsUserFive :: CreateFungibleParams
                fungibleParamsUserFive = CreateFungibleParams
                  { userPkh: userFivePkh, amount: BigInt.fromInt 600 }

              ContractResult
                { txHash: fungibleTxHashUserFive
                , symbol: fungibleSymbolUserFive
                , tokenName: fungibleTokenNameUserFive
                } <- createFungible fungibleParamsUserFive

              void $ awaitTxConfirmedWithTimeout (Seconds 600.0)
                fungibleTxHashUserFour
              void $ waitNSlots (Natural.fromInt' 3)

            -- ************************************************** --
            -- ************************************************** --
            -- * User three (walletThree) votes on proposal one * --
            withKeyWallet walletThree do
              logInfo'
                "Running in wallet three - voting on proposal created by walletOne"

              let
                voteParams :: VoteOnProposalParams
                voteParams = VoteOnProposalParams
                  { configSymbol: configSymbol
                  , configTokenName: configTokenName
                  , tallySymbol: proposalOneSymbol
                  -- Vote datum fields
                  , proposalTokenName: proposalOneTokenName
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

            -- ************************************************ --
            -- ************************************************ --
            -- * User four (walletFour) votes on proposal one * --
            -- withKeyWallet walletFour do
            --   logInfo'
            --     "Running in wallet four - voting on proposal created by walletOne"

            --   let
            --     voteParams :: VoteOnProposalParams
            --     voteParams = VoteOnProposalParams
            --       { configSymbol: configSymbol
            --       , configTokenName: configTokenName
            --       , tallySymbol: proposalOneSymbol
            --       -- Vote datum fields
            --       , proposalTokenName: proposalOneTokenName
            --       , voteDirection: VoteDirection'For
            --       , returnAda: (BigInt.fromInt 0)
            --       }

            --   VoteOnProposalResult
            --     { txHash: voteOnProposalTxHash
            --     , symbol: voteOnProposalSymbol
            --     } <- voteOnProposal voteParams

            --   void $ awaitTxConfirmedWithTimeout (Seconds 600.0)
            --     voteOnProposalTxHash
            --   void $ waitNSlots (Natural.fromInt' 3)

            -- ********************************************************* --
            -- ********************************************************* --
            -- * User one (walletOne) counts the votes on proposal one * --

            withKeyWallet walletOne do
              logInfo'
                "Running in wallet one - counting the votes on the proposal created by walletOne"

              let
                countVoteParams :: CountVoteParams
                countVoteParams = CountVoteParams
                  { configSymbol
                  , configTokenName
                  , tallySymbol: proposalOneSymbol
                  , proposalTokenName: proposalOneTokenName
                  }

              countVoteTxHash <- countVote countVoteParams

              void $ awaitTxConfirmedWithTimeout (Seconds 600.0)
                countVoteTxHash
              void $ waitNSlots (Natural.fromInt' 3)

            -- ************************************************************ --
            -- * User one executes the effect of the proposal, that is to * --
            -- * disburse treasury funds to the 'General' payment address * --
            -- * specified in the 'TallyStateDatum'. In this case that is * --
            -- * 'walletTwoAddress', the address of the user 'walletTwo'. * --
            withKeyWallet walletOne do
              logInfo'
                "Running in wallet one - executing the treasury general effect"

              let
                treasuryGeneralParams :: TreasuryParams
                treasuryGeneralParams = TreasuryParams
                  { configSymbol
                  , configTokenName
                  , proposalTokenName: proposalOneTokenName
                  , tallySymbol: proposalOneSymbol
                  , treasurySymbol: treasuryFundSymbol
                  }

              treasuryTxHash <- treasuryGeneral treasuryGeneralParams

              void $ awaitTxConfirmedWithTimeout (Seconds 600.0) treasuryTxHash
              void $ waitNSlots (Natural.fromInt' 3)

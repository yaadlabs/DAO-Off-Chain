module Dao.Web.Api
  ( cancelVote
  , countVote
  , createConfig
  , createFungible
  , createIndex
  , createProposal
  , createVotePass
  , finalize
  , initialize
  , treasuryGeneral
  , treasuryTrip
  , upgradeConfig
  , voteOnProposal
  )
  where

import Prelude

import Contract.Config
  ( NetworkId(TestnetId, MainnetId)
  , emptyHooks
  , defaultSynchronizationParams
  , defaultTimeParams
  ) as Ctl
import Contract.JsSdk (mkContractEnvJS, stopContractEnvJS)
import Contract.Monad (ContractEnv)
import Control.Promise (Promise)
import Ctl.Internal.Contract.QueryBackend (mkBlockfrostBackendParams) as Ctl
import Ctl.Internal.Wallet.Spec (WalletSpec(ConnectToNami)) as Ctl
import Dao.Web.Call (mkContractCall2)
import Dao.Web.Types
  ( CancelVoteParams
  , ContractResult
  , CountVoteParams
  , CreateConfigParams
  , CreateFungibleParams
  , CreateProposalParams
  , CtlConfig(CtlConfig)
  , PaymentPubKeyHash
  , TokenName
  , TransactionHash
  , TreasuryParams
  , UpgradeConfigParams
  , VoteOnProposalParams
  , VoteOnProposalResult
  )
import Dao.Workflow.CancelVote (cancelVote) as Dao
import Dao.Workflow.CountVote (countVote) as Dao
import Dao.Workflow.CreateConfig (createConfig) as Dao
import Dao.Workflow.CreateFungible (createFungible) as Dao
import Dao.Workflow.CreateIndex (createIndex) as Dao
import Dao.Workflow.CreateProposal (createProposal) as Dao
import Dao.Workflow.CreateVotePass (createVotePass) as Dao
import Dao.Workflow.TreasuryGeneral (treasuryGeneral) as Dao
import Dao.Workflow.TreasuryTrip (treasuryTrip) as Dao
import Dao.Workflow.UpgradeConfig (upgradeConfig) as Dao
import Dao.Workflow.VoteOnProposal (voteOnProposal) as Dao
import Data.Function.Uncurried (Fn1, Fn2)
import Data.Log.Level (LogLevel(Trace))
import Data.Maybe (Maybe(Just, Nothing))
import Data.UInt as UInt

initialize :: CtlConfig -> (Promise ContractEnv)
initialize = mkContractEnvJS <<< mkContractParams
  where
    mkContractParams (CtlConfig config) =
      let
        networkId = case config.network of
          "preview" -> Ctl.TestnetId
          "preprod" -> Ctl.TestnetId
          "mainnet" -> Ctl.MainnetId
          _ -> Ctl.TestnetId
        blockfrostConfig =
          { port: UInt.fromInt 443
          , host: "cardano-" <> config.network <> ".blockfrost.io"
          , secure: true
          , path: Just "/api/v0"
          }
        backendParams = Ctl.mkBlockfrostBackendParams 
          { blockfrostConfig: blockfrostConfig
          , blockfrostApiKey: Just config.blockfrostApiKey
          , confirmTxDelay: Nothing
          }
      in
        { backendParams
        , networkId
        , logLevel: Trace
        , walletSpec: Just Ctl.ConnectToNami
        , customLogger: Nothing
        , suppressLogs: false
        , hooks: Ctl.emptyHooks
        , synchronizationParams: Ctl.defaultSynchronizationParams
        , timeParams: Ctl.defaultTimeParams
        }

finalize :: Fn1 ContractEnv (Promise Unit)
finalize = stopContractEnvJS

createConfig :: Fn2 ContractEnv CreateConfigParams (Promise ContractResult)
createConfig = mkContractCall2 Dao.createConfig

createIndex :: Fn2 ContractEnv TokenName (Promise ContractResult)
createIndex = mkContractCall2 Dao.createIndex

createProposal :: Fn2 ContractEnv CreateProposalParams (Promise ContractResult)
createProposal = mkContractCall2 Dao.createProposal

voteOnProposal :: Fn2 ContractEnv VoteOnProposalParams (Promise VoteOnProposalResult)
voteOnProposal = mkContractCall2 Dao.voteOnProposal

upgradeConfig :: Fn2 ContractEnv UpgradeConfigParams (Promise TransactionHash)
upgradeConfig = mkContractCall2 Dao.upgradeConfig

countVote :: Fn2 ContractEnv CountVoteParams (Promise TransactionHash)
countVote = mkContractCall2 Dao.countVote

cancelVote :: Fn2 ContractEnv CancelVoteParams (Promise TransactionHash)
cancelVote = mkContractCall2 Dao.cancelVote

treasuryGeneral :: Fn2 ContractEnv TreasuryParams (Promise TransactionHash)
treasuryGeneral = mkContractCall2 Dao.treasuryGeneral

createVotePass :: Fn2 ContractEnv PaymentPubKeyHash (Promise ContractResult)
createVotePass = mkContractCall2 Dao.createVotePass

createFungible :: Fn2 ContractEnv CreateFungibleParams (Promise ContractResult)
createFungible = mkContractCall2 Dao.createFungible

treasuryTrip :: Fn2 ContractEnv TreasuryParams (Promise TransactionHash)
treasuryTrip = mkContractCall2 Dao.treasuryTrip

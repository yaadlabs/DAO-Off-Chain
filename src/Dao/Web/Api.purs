module Dao.Web.Api
  ( module WebTypes
  , cancelVote
  , countVote
  , createConfig
  , createFungible
  , createIndexConfig
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

import Contract.Chain (waitNSlots) as Ctl
import Contract.Config
  ( NetworkId(TestnetId, MainnetId)
  , emptyHooks
  , defaultSynchronizationParams
  , defaultTimeParams
  ) as Ctl
import Contract.JsSdk (mkContractEnvJS, stopContractEnvJS) as Ctl
import Contract.Log (logInfo') as Ctl
import Contract.Monad (ContractEnv, liftContractM, runContractInEnv) as Ctl
import Contract.Numeric.Natural (fromInt') as Natural
import Contract.Transaction (awaitTxConfirmedWithTimeout) as Ctl
import Control.Promise (Promise, fromAff)
import Contract.Transaction (awaitTxConfirmedWithTimeout)
import Contract.Value (TokenName, adaSymbol, adaToken, scriptCurrencySymbol) as Value
import Ctl.Internal.Contract.QueryBackend (mkBlockfrostBackendParams) as Ctl
import Ctl.Internal.Wallet.Spec (WalletSpec(ConnectToNami)) as Ctl
import Dao.Component.Config.Params (CreateConfigParams(CreateConfigParams)) as Component
import Dao.Web.Conversion (convertJsToPs) as Conversion
import Dao.Scripts.Policy.Fungible (fungiblePolicy) as Scripts
import Dao.Scripts.Policy.VoteNft (voteNftPolicy) as Scripts
import Dao.Utils.Contract (ContractResult(ContractResult)) as Utils
import Dao.Utils.Value (mkTokenName) as Utils
import Dao.Web.Call (mkContractCall1, mkContractCall2, contractCallOneArg, contractCallTwoArgs)
import Dao.Web.Types
  ( CancelVoteParams
  , ContractResult
  , CountVoteParams
  , CreateConfigParams
  , CreateFungibleParams
  , CreateProposalParams
  , CtlConfig(CtlConfig)
  , JsMaybe
  , PaymentPubKeyHash
  , QueryProposalParams
  , QueryResult
  , TokenName
  , TransactionHash
  , TreasuryParams
  , UpgradeConfigParams
  , VoteOnProposalParams
  , VoteOnProposalResult
  )
import Dao.Web.Types (ProposalType(..)) as WebTypes
import Dao.Workflow.CancelVote (cancelVote) as Dao
import Dao.Workflow.CountVote (countVote) as Dao
import Dao.Workflow.CreateConfig (createConfig) as Dao
import Dao.Workflow.CreateFungible (createFungible) as Dao
import Dao.Workflow.CreateIndex (createIndex) as Dao
import Dao.Workflow.CreateProposal (createProposal) as Dao
import Dao.Workflow.CreateVotePass (createVotePass) as Dao
import Dao.Workflow.QueryProposal
  ( getAllActiveProposals
  , getAllExpiredProposals
  , getAllGeneralProposals
  , getAllProposals
  , getAllSuccessfulProposals
  , getAllTripProposals
  , getAllUpgradeProposals
  , getProposalByTokenName
  ) as Dao
import Dao.Workflow.TreasuryGeneral (treasuryGeneral) as Dao
import Dao.Workflow.TreasuryTrip (treasuryTrip) as Dao
import Dao.Workflow.UpgradeConfig (upgradeConfig) as Dao
import Dao.Workflow.VoteOnProposal (voteOnProposal) as Dao
import Data.Function.Uncurried (Fn0, Fn1, Fn2, mkFn2)
import Data.Log.Level (LogLevel(Trace))
import Data.Maybe (Maybe(Just, Nothing))
import Data.UInt as UInt
import Data.Newtype (unwrap)
import Data.Time.Duration (Seconds(Seconds))
import Effect.Aff.Compat (EffectFn1, EffectFn2)
import Effect.Unsafe (unsafePerformEffect)
import JS.BigInt (fromInt) as BigInt
import Record (merge)

initialize :: CtlConfig -> (Promise Ctl.ContractEnv)
initialize = Ctl.mkContractEnvJS <<< mkContractParams
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

finalize :: Fn1 Ctl.ContractEnv (Promise Unit)
finalize = Ctl.stopContractEnvJS

createIndex :: Fn2 Ctl.ContractEnv TokenName (Promise ContractResult)
createIndex = mkContractCall2 Dao.createIndex

createConfig :: Fn2 Ctl.ContractEnv CreateConfigParams (Promise ContractResult)
createConfig = mkContractCall2 Dao.createConfig

-- | Create both the index and config with default config params
createIndexConfig :: Fn1 Ctl.ContractEnv (Promise ContractResult)
createIndexConfig = mkContractCall1 createIndexConfig'
  where 
    createIndexConfig' = do
      indexTokenName <-
        Ctl.liftContractM "Could not make index token name" $ Utils.mkTokenName "index1"
      Utils.ContractResult
        { txHash: createIndexTxHash
        , symbol: indexSymbol
        , tokenName: indexTokenName
        } <- Dao.createIndex indexTokenName

      void $ Ctl.awaitTxConfirmedWithTimeout (Seconds 600.0) createIndexTxHash
      void $ Ctl.waitNSlots (Natural.fromInt' 3)
        
      Ctl.logInfo' $ "Index created with symbol: " <> show indexSymbol <> " and token name: " <> show indexTokenName

      voteNftPolicy <- Scripts.voteNftPolicy
      fungiblePolicy <- Scripts.fungiblePolicy
      voteFungibleTokenName <-
        Ctl.liftContractM "Could not make voteNft token name" $ Utils.mkTokenName "vote_fungible"

      let 
        voteNftSymbol = Value.scriptCurrencySymbol voteNftPolicy
        voteFungibleCurrencySymbol = Value.scriptCurrencySymbol fungiblePolicy
        params = Component.CreateConfigParams 
          { configTokenName: Value.adaToken
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
          , voteTokenName: Value.adaToken
          , voteFungibleCurrencySymbol
          , voteFungibleTokenName
          , voteNftSymbol
          , fungibleVotePercent: BigInt.fromInt 10
          -- Index needed for making tallyNft
          , indexSymbol: indexSymbol
          , indexTokenName: indexTokenName
          }
          
      Dao.createConfig params

createProposal :: Fn2 Ctl.ContractEnv CreateProposalParams (Promise ContractResult)
createProposal = mkContractCall2 Dao.createProposal

voteOnProposal :: Fn2 Ctl.ContractEnv VoteOnProposalParams (Promise VoteOnProposalResult)
voteOnProposal = mkContractCall2 Dao.voteOnProposal

upgradeConfig :: Fn2 Ctl.ContractEnv UpgradeConfigParams (Promise TransactionHash)
upgradeConfig = mkContractCall2 Dao.upgradeConfig

countVote :: Fn2 Ctl.ContractEnv CountVoteParams (Promise TransactionHash)
countVote = mkContractCall2 Dao.countVote

cancelVote :: Fn2 Ctl.ContractEnv CancelVoteParams (Promise TransactionHash)
cancelVote = mkContractCall2 Dao.cancelVote

treasuryGeneral :: Fn2 Ctl.ContractEnv TreasuryParams (Promise TransactionHash)
treasuryGeneral = mkContractCall2 Dao.treasuryGeneral

createVotePass :: Fn2 Ctl.ContractEnv PaymentPubKeyHash (Promise ContractResult)
createVotePass = mkContractCall2 Dao.createVotePass

createFungible :: Fn2 Ctl.ContractEnv CreateFungibleParams (Promise ContractResult)
createFungible = mkContractCall2 Dao.createFungible

treasuryTrip :: Fn2 Ctl.ContractEnv TreasuryParams (Promise TransactionHash)
treasuryTrip = mkContractCall2 Dao.treasuryTrip

getAllProposals ::
  Ctl.ContractEnv ->
  EffectFn1 QueryProposalParams (Promise (Array QueryResult))
getAllProposals env = contractCallOneArg env Dao.getAllProposals

getAllGeneralProposals ::
  Ctl.ContractEnv ->
  EffectFn1 QueryProposalParams (Promise (Array QueryResult))
getAllGeneralProposals env = contractCallOneArg env Dao.getAllGeneralProposals

getAllTripProposals ::
  Ctl.ContractEnv ->
  EffectFn1 QueryProposalParams (Promise (Array QueryResult))
getAllTripProposals env = contractCallOneArg env Dao.getAllTripProposals

getAllUpgradeProposals ::
  Ctl.ContractEnv ->
  EffectFn1 QueryProposalParams (Promise (Array QueryResult))
getAllUpgradeProposals env = contractCallOneArg env Dao.getAllUpgradeProposals

getAllActiveProposals ::
  Ctl.ContractEnv ->
  EffectFn1 QueryProposalParams (Promise (Array QueryResult))
getAllActiveProposals env = contractCallOneArg env Dao.getAllActiveProposals

getAllExpiredProposals ::
  Ctl.ContractEnv ->
  EffectFn1 QueryProposalParams (Promise (Array QueryResult))
getAllExpiredProposals env = contractCallOneArg env Dao.getAllExpiredProposals

getAllSuccessfulProposals ::
  Ctl.ContractEnv ->
  EffectFn1 QueryProposalParams (Promise (Array QueryResult))
getAllSuccessfulProposals env = contractCallOneArg env
  Dao.getAllSuccessfulProposals

getProposalByTokenName ::
  Ctl.ContractEnv ->
  EffectFn2 QueryProposalParams TokenName (Promise (JsMaybe QueryResult))
getProposalByTokenName env = contractCallTwoArgs env Dao.getProposalByTokenName

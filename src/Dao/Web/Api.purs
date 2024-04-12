module Dao.Web.Api
  ( cancelVote
  , countVote
  , createConfig
  , createFungible
  , createIndex
  , createIndexConfig
  , createProposal
  , createTreasuryFund
  , createVotePass
  , deployAllReferenceScripts
  , deployReferenceScriptsOne
  , deployReferenceScriptsTwo
  , deployReferenceScriptsThree
  , finalize
  , getAllActiveProposals
  , getAllExpiredProposals
  , getAllGeneralProposals
  , getAllProposals
  , getAllSuccessfulProposals
  , getAllTripProposals
  , getAllUpgradeProposals
  , getProposalByTokenName
  , initialize
  , module WebTypes
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
import Contract.Monad (ContractEnv, liftContractM) as Ctl
import Contract.Numeric.Natural (fromInt') as Natural
import Contract.Transaction (awaitTxConfirmedWithTimeout) as Ctl
import Contract.Value (adaToken, scriptCurrencySymbol) as Value
import Control.Promise (Promise)
import Ctl.Internal.Contract.QueryBackend (mkBlockfrostBackendParams) as Ctl
import Ctl.Internal.Wallet.Spec (WalletSpec(ConnectToNami)) as Ctl
import Dao.Component.Config.Params (CreateConfigParams(CreateConfigParams), mkValidatorConfig) as Component
import Dao.Scripts.Policy (fungiblePolicy) as Scripts
import Dao.Scripts.Policy (voteNftPolicy) as Scripts
import Dao.Utils.Address (addressToPaymentPubKeyHash) as Utils
import Dao.Utils.Contract (ContractResult(ContractResult)) as Utils
import Dao.Utils.Value (mkTokenName) as Utils
import Dao.Web.Call (mkContractCall1, mkContractCall2, mkContractCall3)
import Dao.Web.Types
  ( Address
  , CancelVoteParams
  , ContractResult
  , CountVoteParams
  , CreateConfigParams
  , CreateConfigResult
  , CreateFungibleParams
  , CreateProposalParams
  , CreateTreasuryFundParams
  , CtlConfig(CtlConfig)
  , JsMaybe
  , QueryProposalParams
  , QueryResult
  , TokenName
  , TransactionHash
  , TreasuryParams
  , UpgradeConfigParams
  , ValidatorParams
  , VoteOnProposalParams
  , VoteOnProposalResult
  )
import Dao.Web.Types (ProposalType(..), VoteDirection(..)) as WebTypes
import Dao.Workflow.CancelVote (cancelVote) as Dao
import Dao.Workflow.CountVote (countVote) as Dao
import Dao.Workflow.CreateConfig (createConfig) as Dao
import Dao.Workflow.CreateFungible (createFungible) as Dao
import Dao.Workflow.CreateIndex (createIndex) as Dao
import Dao.Workflow.CreateProposal (createProposal) as Dao
import Dao.Workflow.CreateTreasuryFund (createTreasuryFund) as Dao
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
import Dao.Workflow.ReferenceScripts
  ( deployReferenceScriptsOne
  , deployReferenceScriptsTwo
  , deployReferenceScriptsThree
  ) as Dao
import Dao.Workflow.TreasuryGeneral (treasuryGeneral) as Dao
import Dao.Workflow.TreasuryTrip (treasuryTrip) as Dao
import Dao.Workflow.UpgradeConfig (upgradeConfig) as Dao
import Dao.Workflow.VoteOnProposal (voteOnProposal) as Dao
import Data.Function.Uncurried (Fn1, Fn2, Fn3)
import Data.Log.Level (LogLevel(Trace))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Time.Duration (Seconds(Seconds))
import Data.UInt as UInt
import JS.BigInt (fromInt) as BigInt
import ScriptArguments.Types (ValidatorParams(ValidatorParams)) as ScriptArguments

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

createConfig :: Fn2 Ctl.ContractEnv CreateConfigParams (Promise CreateConfigResult)
createConfig = mkContractCall2 Dao.createConfig

-- | Create both the index and config with default config params
createIndexConfig :: Fn1 Ctl.ContractEnv (Promise CreateConfigResult)
createIndexConfig = mkContractCall1 createIndexConfig'
  where 
    createIndexConfig' = do
      indexTokenName <-
        Ctl.liftContractM "Could not make index token name" $ Utils.mkTokenName "index1"
      Utils.ContractResult
        { txHash: createIndexTxHash
        , symbol: indexSymbol
        , tokenName: indexTokenName'
        } <- Dao.createIndex indexTokenName

      void $ Ctl.awaitTxConfirmedWithTimeout (Seconds 600.0) createIndexTxHash
      void $ Ctl.waitNSlots (Natural.fromInt' 3)
        
      Ctl.logInfo' $ "Index created with symbol: " <> show indexSymbol <> " and token name: " <> show indexTokenName'

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
          , indexTokenName: indexTokenName'
          }
          
      Dao.createConfig params

deployReferenceScriptsOne :: Fn2 Ctl.ContractEnv ValidatorParams (Promise TransactionHash)
deployReferenceScriptsOne = 
  mkContractCall2
    \(ScriptArguments.ValidatorParams { vpConfigSymbol, vpConfigTokenName }) ->
      Dao.deployReferenceScriptsOne (Component.mkValidatorConfig vpConfigSymbol vpConfigTokenName)

deployReferenceScriptsTwo :: Fn2 Ctl.ContractEnv ValidatorParams (Promise TransactionHash)
deployReferenceScriptsTwo = 
  mkContractCall2
    \(ScriptArguments.ValidatorParams { vpConfigSymbol, vpConfigTokenName }) ->
      Dao.deployReferenceScriptsTwo (Component.mkValidatorConfig vpConfigSymbol vpConfigTokenName)

deployReferenceScriptsThree :: Fn2 Ctl.ContractEnv ValidatorParams (Promise TransactionHash)
deployReferenceScriptsThree = 
  mkContractCall2
    \(ScriptArguments.ValidatorParams { vpConfigSymbol, vpConfigTokenName }) ->
      Dao.deployReferenceScriptsThree (Component.mkValidatorConfig vpConfigSymbol vpConfigTokenName)

deployAllReferenceScripts :: Fn2 Ctl.ContractEnv ValidatorParams (Promise (Array TransactionHash))
deployAllReferenceScripts = 
  mkContractCall2
    \(ScriptArguments.ValidatorParams { vpConfigSymbol, vpConfigTokenName }) -> do
      let 
        validatorConfig = Component.mkValidatorConfig vpConfigSymbol vpConfigTokenName
      txId1 <- Dao.deployReferenceScriptsOne validatorConfig
      txId2 <- Dao.deployReferenceScriptsTwo validatorConfig
      txid3 <- Dao.deployReferenceScriptsThree validatorConfig
      pure [txId1, txId2, txid3]

createProposal :: Fn2 Ctl.ContractEnv CreateProposalParams (Promise ContractResult)
createProposal = mkContractCall2 Dao.createProposal

createTreasuryFund :: Fn2 Ctl.ContractEnv CreateTreasuryFundParams (Promise ContractResult)
createTreasuryFund = mkContractCall2 Dao.createTreasuryFund

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

createVotePass :: Fn2 Ctl.ContractEnv Address (Promise ContractResult)
createVotePass = mkContractCall2 $ \address -> do
  pkh <- Ctl.liftContractM "Could not convert address to key" $
    Utils.addressToPaymentPubKeyHash address
  Dao.createVotePass pkh

createFungible :: Fn2 Ctl.ContractEnv CreateFungibleParams (Promise ContractResult)
createFungible = mkContractCall2 Dao.createFungible

treasuryTrip :: Fn2 Ctl.ContractEnv TreasuryParams (Promise TransactionHash)
treasuryTrip = mkContractCall2 Dao.treasuryTrip

getAllProposals :: Fn2 Ctl.ContractEnv QueryProposalParams (Promise (Array QueryResult))
getAllProposals = mkContractCall2 Dao.getAllProposals

getAllGeneralProposals :: Fn2 Ctl.ContractEnv QueryProposalParams (Promise (Array QueryResult))
getAllGeneralProposals = mkContractCall2 Dao.getAllGeneralProposals

getAllTripProposals :: Fn2 Ctl.ContractEnv QueryProposalParams (Promise (Array QueryResult))
getAllTripProposals = mkContractCall2 Dao.getAllTripProposals

getAllUpgradeProposals :: Fn2 Ctl.ContractEnv QueryProposalParams (Promise (Array QueryResult))
getAllUpgradeProposals = mkContractCall2 Dao.getAllUpgradeProposals

getAllActiveProposals :: Fn2 Ctl.ContractEnv QueryProposalParams (Promise (Array QueryResult))
getAllActiveProposals = mkContractCall2 Dao.getAllActiveProposals

getAllExpiredProposals :: Fn2 Ctl.ContractEnv QueryProposalParams (Promise (Array QueryResult))
getAllExpiredProposals = mkContractCall2 Dao.getAllExpiredProposals

getAllSuccessfulProposals :: Fn2 Ctl.ContractEnv QueryProposalParams (Promise (Array QueryResult))
getAllSuccessfulProposals = mkContractCall2 Dao.getAllSuccessfulProposals

getProposalByTokenName :: Fn3 Ctl.ContractEnv QueryProposalParams TokenName (Promise (JsMaybe QueryResult))
getProposalByTokenName = mkContractCall3 Dao.getProposalByTokenName

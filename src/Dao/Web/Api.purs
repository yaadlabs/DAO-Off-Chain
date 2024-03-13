module Dao.Web.Api
  ( cancelVote
  , config
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

import Contract.Config (ContractParams)
import Contract.JsSdk (mkContractEnvJS, stopContractEnvJS)
import Contract.Monad (ContractEnv)
import Control.Promise (Promise)
import Dao.Web.Call (mkContractCall2, mkContractCall2)
import Dao.Web.Ctl (contractConfig)
import Dao.Web.Types
  ( CancelVoteParams
  , ContractResult
  , CountVoteParams
  , CreateConfigParams
  , CreateFungibleParams
  , CreateProposalParams
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
import Effect.Aff.Compat (EffectFn1)

initialize :: Fn1 ContractParams (Promise ContractEnv)
initialize = mkContractEnvJS

finalize :: Fn1 ContractEnv (Promise Unit)
finalize = stopContractEnvJS

config :: ContractParams
config = contractConfig

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

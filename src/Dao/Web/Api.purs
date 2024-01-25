module Dao.Web.Api where

import Contract.Monad (ContractEnv)
import Control.Promise (Promise)
import Dao.Web.Call (contractCallOneArg)
import Dao.Web.Types
  ( CancelVoteParams
  , ContractResult
  , CountVoteParams
  , CreateConfigParams
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
import Dao.Workflow.CreateIndex (createIndex) as Dao
import Dao.Workflow.CreateProposal (createProposal) as Dao
import Dao.Workflow.CreateVotePass (createVotePass) as Dao
import Dao.Workflow.TreasuryGeneral (treasuryGeneral) as Dao
import Dao.Workflow.TreasuryTrip (treasuryTrip) as Dao
import Dao.Workflow.UpgradeConfig (upgradeConfig) as Dao
import Dao.Workflow.VoteOnProposal (voteOnProposal) as Dao
import Effect.Aff.Compat (EffectFn1)

createConfig ::
  ContractEnv ->
  EffectFn1 CreateConfigParams (Promise ContractResult)
createConfig env = contractCallOneArg env Dao.createConfig

createIndex ::
  ContractEnv ->
  EffectFn1 TokenName (Promise ContractResult)
createIndex env = contractCallOneArg env Dao.createIndex

createProposal ::
  ContractEnv ->
  EffectFn1 CreateProposalParams (Promise ContractResult)
createProposal env = contractCallOneArg env Dao.createProposal

voteOnProposal ::
  ContractEnv ->
  EffectFn1 VoteOnProposalParams (Promise VoteOnProposalResult)
voteOnProposal env = contractCallOneArg env Dao.voteOnProposal

upgradeConfig ::
  ContractEnv ->
  EffectFn1 UpgradeConfigParams (Promise TransactionHash)
upgradeConfig env = contractCallOneArg env Dao.upgradeConfig

countVote ::
  ContractEnv ->
  EffectFn1 CountVoteParams (Promise TransactionHash)
countVote env = contractCallOneArg env Dao.countVote

cancelVote ::
  ContractEnv ->
  EffectFn1 CancelVoteParams (Promise TransactionHash)
cancelVote env = contractCallOneArg env Dao.cancelVote

treasuryGeneral ::
  ContractEnv ->
  EffectFn1 TreasuryParams (Promise TransactionHash)
treasuryGeneral env = contractCallOneArg env Dao.treasuryGeneral

createVotePass ::
  ContractEnv ->
  EffectFn1 PaymentPubKeyHash (Promise ContractResult)
createVotePass env = contractCallOneArg env Dao.createVotePass

treasuryTrip ::
  ContractEnv ->
  EffectFn1 TreasuryParams (Promise TransactionHash)
treasuryTrip env = contractCallOneArg env Dao.treasuryTrip

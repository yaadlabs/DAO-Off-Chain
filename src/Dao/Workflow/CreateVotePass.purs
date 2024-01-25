{-|
Module: Dao.Workflow.CreateVotePass
Description: Contract for creating token corresponding to the 'voteNft' field of the config
-}
module Dao.Workflow.CreateVotePass (createVotePass) where

import Contract.Address (PaymentPubKeyHash)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.Prelude
  ( type (/\)
  , bind
  , discard
  , mconcat
  , one
  , pure
  , ($)
  , (/\)
  )
import Contract.ScriptLookups as Lookups
import Contract.Transaction
  ( TransactionHash
  , submitTxFromConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , Value
  , scriptCurrencySymbol
  )
import Contract.Value (singleton) as Value
import Dao.Scripts.Policy.VoteNft (voteNftPolicy)
import Dao.Utils.Contract (ContractResult(ContractResult))
import Dao.Utils.Value (mkTokenName)

-- | Contract for creating token corresponding to the 'voteNft' field of the config
-- | This token acts as a pass for voting on a proposal
createVotePass ::
  PaymentPubKeyHash ->
  Contract ContractResult
createVotePass userPkh = do
  logInfo' "Entering createVotePass transaction"

  voteNftPolicy' <- voteNftPolicy

  voteNftTokenName :: TokenName <-
    liftContractM "Could not make voteNft token name" $ mkTokenName "vote_pass"

  let
    voteNftSymbol :: CurrencySymbol
    voteNftSymbol = scriptCurrencySymbol voteNftPolicy'

    voteNftValue :: Value
    voteNftValue = Value.singleton voteNftSymbol voteNftTokenName one

    lookups :: Lookups.ScriptLookups
    lookups = mconcat [ Lookups.mintingPolicy voteNftPolicy' ]

    constraints :: Constraints.TxConstraints
    constraints = mconcat
      [ Constraints.mustMintValue voteNftValue
      , Constraints.mustPayToPubKey userPkh voteNftValue
      ]

  txHash <- submitTxFromConstraints lookups constraints

  pure $ ContractResult
    { txHash, symbol: voteNftSymbol, tokenName: voteNftTokenName }

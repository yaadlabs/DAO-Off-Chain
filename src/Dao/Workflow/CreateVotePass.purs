{-|
Module: Dao.Workflow.CreateVotePass
Description: Contract for creating token corresponding to the 'voteNft' field of the config
-}
module Dao.Workflow.CreateVotePass (createVotePass) where

import Contract.Address (PaymentPubKeyHash)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.PlutusData (Datum(Datum), toData)
import Contract.Prelude
  ( type (/\)
  , bind
  , discard
  , mconcat
  , one
  , pure
  , unwrap
  , ($)
  , (/\)
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts
  ( MintingPolicy
  , ScriptHash
  , Validator
  , ValidatorHash
  , validatorHash
  )
import Contract.Transaction
  ( TransactionHash
  , TransactionInput
  , TransactionOutputWithRefScript
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
import Contract.Wallet (ownPaymentPubKeyHash)
import Dao.Utils.Value (mkTokenName)
import Scripts.VoteNft (voteNftPolicy)

-- | Contract for creating token corresponding to the 'voteNft' field of the config
-- | This token acts as a pass for voting on a proposal
createVotePass :: Contract (TransactionHash /\ CurrencySymbol /\ TokenName)
createVotePass = do
  logInfo' "Entering createVotePass transaction"

  voteNftPolicy' <- voteNftPolicy

  userPkh :: PaymentPubKeyHash <- liftedM "Could not get pkh"
    ownPaymentPubKeyHash
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
    constraints = mconcat [ Constraints.mustPayToPubKey userPkh voteNftValue ]

  txHash <- submitTxFromConstraints lookups constraints

  pure (txHash /\ voteNftSymbol /\ voteNftTokenName)

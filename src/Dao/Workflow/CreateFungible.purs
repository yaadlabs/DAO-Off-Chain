{-|
Module: Dao.Workflow.CreateFungible
Description: Contract for creating token corresponding to the 'voteFungibleCurrencySymbol' field of the config
-}
module Dao.Workflow.CreateFungible (createFungible) where

import Contract.Address (PaymentPubKeyHash)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.Prelude
  ( type (/\)
  , bind
  , discard
  , mconcat
  , pure
  , ($)
  , (/\)
  , (<)
  , (>)
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy)
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
import Dao.Utils.Error (guardContract)
import Dao.Utils.Value (mkTokenName)
import JS.BigInt (BigInt, fromInt)
import Scripts.FungiblePolicy (unappliedFungiblePolicyDebug)

-- | Contract for creating token corresponding to the 'voteFungibleCurrencySymbol' field of the config
-- | This token acts as a multiplier of a user's voting weight
createFungible ::
  PaymentPubKeyHash ->
  BigInt ->
  Contract (TransactionHash /\ CurrencySymbol /\ TokenName)
createFungible userPkh tokenAmount = do
  logInfo' "Entering createVotePass transaction"

  -- Script sets these arbitrary bounds on number of tokens allowed
  guardContract "Token amount must be greater than 0"
    (tokenAmount > (fromInt 0))
  guardContract "Token amount must be less than 500"
    (tokenAmount < (fromInt 500))

  appliedFungiblePolicy :: MintingPolicy <- unappliedFungiblePolicyDebug
    tokenAmount

  fungibleTokenName :: TokenName <-
    liftContractM "Could not make voteNft token name" $ mkTokenName
      "vote_fungible"

  let
    fungibleSymbol :: CurrencySymbol
    fungibleSymbol = scriptCurrencySymbol appliedFungiblePolicy

    fungibleValue :: Value
    fungibleValue = Value.singleton fungibleSymbol fungibleTokenName tokenAmount

    lookups :: Lookups.ScriptLookups
    lookups = mconcat [ Lookups.mintingPolicy appliedFungiblePolicy ]

    constraints :: Constraints.TxConstraints
    constraints = mconcat
      [ Constraints.mustMintValue fungibleValue
      , Constraints.mustPayToPubKey userPkh fungibleValue
      ]

  txHash <- submitTxFromConstraints lookups constraints

  pure (txHash /\ fungibleSymbol /\ fungibleTokenName)

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
  , unwrap
  , (#)
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
import Dao.Component.Fungible.Params (CreateFungibleParams)
import Dao.Scripts.Policy.Fungible (fungiblePolicy)
import Dao.Utils.Contract (ContractResult(ContractResult))
import Dao.Utils.Error (guardContract)
import Dao.Utils.Value (mkTokenName)
import JS.BigInt (BigInt, fromInt)

-- | Contract for creating token corresponding to the 'voteFungibleCurrencySymbol' field of the config
-- | This token acts as a multiplier of a user's voting weight
-- | Uses an 'always-succeed' on-chain script as a placeholder
createFungible :: CreateFungibleParams -> Contract ContractResult
createFungible params' = do
  logInfo' "Entering createFungible transaction"

  let params = params' # unwrap

  fungiblePolicy' :: MintingPolicy <- fungiblePolicy
  fungibleTokenName :: TokenName <-
    liftContractM "Could not make voteNft token name" $ mkTokenName
      "vote_fungible"

  let
    fungibleSymbol :: CurrencySymbol
    fungibleSymbol = scriptCurrencySymbol fungiblePolicy'

    fungibleValue :: Value
    fungibleValue = Value.singleton fungibleSymbol fungibleTokenName
      params.amount

    lookups :: Lookups.ScriptLookups
    lookups = mconcat [ Lookups.mintingPolicy fungiblePolicy' ]

    constraints :: Constraints.TxConstraints
    constraints = mconcat
      [ Constraints.mustMintValue fungibleValue
      , Constraints.mustPayToPubKey params.userPkh fungibleValue
      ]

  txHash <- submitTxFromConstraints lookups constraints

  pure $ ContractResult
    { txHash, symbol: fungibleSymbol, tokenName: fungibleTokenName }

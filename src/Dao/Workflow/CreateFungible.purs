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
import Dao.Scripts.Policy.Fungible (unappliedFungiblePolicyDebug)
import Dao.Utils.Contract (ContractResult(ContractResult))
import Dao.Utils.Error (guardContract)
import Dao.Utils.Value (mkTokenName)
import JS.BigInt (BigInt, fromInt)

-- | Contract for creating token corresponding to the 'voteFungibleCurrencySymbol' field of the config
-- | This token acts as a multiplier of a user's voting weight
createFungible ::
  CreateFungibleParams ->
  Contract ContractResult
createFungible params' = do
  logInfo' "Entering createVotePass transaction"

  let params = params' # unwrap

  -- Script sets these arbitrary bounds on number of tokens allowed
  guardContract "Token amount must be greater than 0"
    (params.amount > (fromInt 0))
  guardContract "Token amount must be less than 500"
    (params.amount < (fromInt 500))

  appliedFungiblePolicy :: MintingPolicy <- unappliedFungiblePolicyDebug
    params.amount

  fungibleTokenName :: TokenName <-
    liftContractM "Could not make voteNft token name" $ mkTokenName
      "vote_fungible"

  let
    fungibleSymbol :: CurrencySymbol
    fungibleSymbol = scriptCurrencySymbol appliedFungiblePolicy

    fungibleValue :: Value
    fungibleValue = Value.singleton fungibleSymbol fungibleTokenName
      params.amount

    lookups :: Lookups.ScriptLookups
    lookups = mconcat [ Lookups.mintingPolicy appliedFungiblePolicy ]

    constraints :: Constraints.TxConstraints
    constraints = mconcat
      [ Constraints.mustMintValue fungibleValue
      , Constraints.mustPayToPubKey params.userPkh fungibleValue
      ]

  txHash <- submitTxFromConstraints lookups constraints

  pure $ ContractResult
    { txHash, symbol: fungibleSymbol, tokenName: fungibleTokenName }

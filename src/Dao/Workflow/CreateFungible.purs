{-|
Module: Dao.Workflow.CreateFungible
Description: Contract for creating token corresponding to the 'voteFungibleCurrencySymbol' field of the config
-}
module Dao.Workflow.CreateFungible (createFungible) where

import Cardano.Types (AssetName, PlutusScript, ScriptHash, Value)
import Cardano.Types.BigNum (fromBigInt) as BigNum
import Cardano.Types.Mint (fromMultiAsset) as Mint
import Cardano.Types.PlutusScript (hash) as PlutusScript
import Cardano.Types.Value (getMultiAsset, singleton) as Value
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.Prelude
  ( bind
  , discard
  , mconcat
  , pure
  , unwrap
  , (#)
  , ($)
  )
import Contract.ScriptLookups as Lookups
import Contract.Transaction (submitTxFromConstraints)
import Contract.TxConstraints as Constraints
import Dao.Component.Fungible.Params (CreateFungibleParams)
import Dao.Scripts.Policy (fungiblePolicy)
import Dao.Utils.Contract (ContractResult(ContractResult))
import Dao.Utils.Value (mkTokenName)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)

-- | Contract for creating token corresponding to the 'voteFungibleCurrencySymbol' field of the config
-- | This token acts as a multiplier of a user's voting weight
-- | Uses an 'always-succeed' on-chain script as a placeholder
createFungible :: CreateFungibleParams -> Contract ContractResult
createFungible params' = do
  logInfo' "Entering createFungible transaction"

  let params = params' # unwrap

  fungiblePolicy' :: PlutusScript <- fungiblePolicy
  fungibleTokenName :: AssetName <-
    liftContractM "Could not make voteNft token name" $ mkTokenName
      "vote_fungible"

  let
    fungibleSymbol :: ScriptHash
    fungibleSymbol = PlutusScript.hash fungiblePolicy'

    fungibleValue :: Value
    fungibleValue =
      -- FIXME: unsafe
      Value.singleton fungibleSymbol fungibleTokenName $ unsafePartial fromJust
        (BigNum.fromBigInt params.amount)

    lookups :: Lookups.ScriptLookups
    lookups = mconcat [ Lookups.plutusMintingPolicy fungiblePolicy' ]

    constraints :: Constraints.TxConstraints
    constraints = mconcat
      [ Constraints.mustMintValue $ Mint.fromMultiAsset $ Value.getMultiAsset
          fungibleValue
      , Constraints.mustPayToPubKey params.userPkh fungibleValue
      ]

  txHash <- submitTxFromConstraints lookups constraints

  pure $ ContractResult
    { txHash, symbol: fungibleSymbol, tokenName: fungibleTokenName }

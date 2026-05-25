{-|
Module: Dao.Workflow.CreateVotePass
Description: Contract for creating token corresponding to the 'voteNft' field of the config
-}
module Dao.Workflow.CreateVotePass (createVotePass) where

import Cardano.Types.BigNum (one) as BigNum
import Cardano.Types.Mint (fromMultiAsset) as Mint
import Cardano.Types.PlutusScript (hash) as PlutusScript
import Contract.Address (PaymentPubKeyHash)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.Prelude
  ( bind
  , discard
  , mconcat
  , pure
  , ($)
  )
import Contract.ScriptLookups as Lookups
import Contract.Transaction (submitTxFromConstraints)
import Contract.TxConstraints as Constraints
import Contract.Value (CurrencySymbol, TokenName, Value)
import Contract.Value (getMultiAsset, singleton) as Value
import Dao.Scripts.Policy (voteNftPolicy)
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
    voteNftSymbol = PlutusScript.hash voteNftPolicy'

    voteNftValue :: Value
    voteNftValue = Value.singleton voteNftSymbol voteNftTokenName BigNum.one

    lookups :: Lookups.ScriptLookups
    lookups = Lookups.plutusMintingPolicy voteNftPolicy'

    constraints :: Constraints.TxConstraints
    constraints = mconcat
      [ Constraints.mustMintValue $ Mint.fromMultiAsset $ Value.getMultiAsset
          voteNftValue
      , Constraints.mustPayToPubKey userPkh voteNftValue
      ]

  txHash <- submitTxFromConstraints lookups constraints

  pure $ ContractResult
    { txHash, symbol: voteNftSymbol, tokenName: voteNftTokenName }

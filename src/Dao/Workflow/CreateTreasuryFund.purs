{-|
Module: Dao.Workflow.CreateTreasuryFund
Description: Contract for creating fund UTXO at treasury validator
-}
module Dao.Workflow.CreateTreasuryFund (createTreasuryFund) where

import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData (unitDatum)
import Contract.Prelude
  ( type (/\)
  , bind
  , discard
  , mconcat
  , one
  , pure
  , ($)
  , (/\)
  , (<>)
  , (>)
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy, Validator, ValidatorHash, validatorHash)
import Contract.Transaction
  ( TransactionHash
  , submitTxFromConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Value
  ( CurrencySymbol
  , Value
  , adaSymbol
  , adaToken
  , scriptCurrencySymbol
  )
import Contract.Value (singleton) as Value
import Dao.Component.Config.Params (mkValidatorConfig)
import Dao.Component.Treasury.Params (TreasuryFundParams)
import Dao.Utils.Error (guardContract)
import Dao.Utils.Query (getAllWalletUtxos)
import Data.Array (head)
import Data.Map as Map
import JS.BigInt (fromInt)
import Scripts.TreasuryPolicy (unappliedTreasuryPolicyDebug)
import Scripts.TreasuryValidator (unappliedTreasuryValidatorDebug)

-- | Contract for creating token corresponding to the 'voteFungibleCurrencySymbol' field of the config
-- | This token acts as a multiplier of a user's voting weight
createTreasuryFund ::
  TreasuryFundParams ->
  Contract (TransactionHash /\ CurrencySymbol)
createTreasuryFund params = do
  logInfo' "Entering createTreasuryFund transaction"

  guardContract "Token amount must be greater than 0"
    (params.adaAmount > (fromInt 0))

  let
    validatorConfig = mkValidatorConfig params.configSymbol
      params.configTokenName

  userUtxos <- getAllWalletUtxos

  (txIn /\ _) <- liftContractM "No UTXOs found"
    $ head
    $ Map.toUnfoldable userUtxos

  appliedTreasuryPolicy :: MintingPolicy <- unappliedTreasuryPolicyDebug txIn
  appliedTreasuryValidator :: Validator <- unappliedTreasuryValidatorDebug
    validatorConfig

  let
    treasuryValidatorHash :: ValidatorHash
    treasuryValidatorHash = validatorHash appliedTreasuryValidator

    treasurySymbol :: CurrencySymbol
    treasurySymbol = scriptCurrencySymbol appliedTreasuryPolicy

    treasuryValue :: Value
    treasuryValue = Value.singleton treasurySymbol adaToken one

    adaValue :: Value
    adaValue = Value.singleton adaSymbol adaToken params.adaAmount

    lookups :: Lookups.ScriptLookups
    lookups = mconcat
      [ Lookups.mintingPolicy appliedTreasuryPolicy
      , Lookups.unspentOutputs userUtxos
      ]

    constraints :: Constraints.TxConstraints
    constraints = mconcat
      [ Constraints.mustMintValue treasuryValue
      , Constraints.mustSpendPubKeyOutput txIn
      , Constraints.mustPayToScript
          treasuryValidatorHash
          unitDatum
          Constraints.DatumInline
          (treasuryValue <> adaValue)
      ]

  txHash <- submitTxFromConstraints lookups constraints

  pure (txHash /\ treasurySymbol)

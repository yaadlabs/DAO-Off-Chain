{-|
Module: Dao.Workflow.CreateTreasuryFund
Description: Contract for creating fund UTXO at treasury validator
-}
module Dao.Workflow.CreateTreasuryFund (createTreasuryFund) where

import Cardano.Plutus.Types.TokenName (adaToken)
import Cardano.Types (PlutusScript, ScriptHash, Value)
import Cardano.Types.BigNum (fromBigInt, one) as BigNum
import Cardano.Types.Mint (fromMultiAsset) as Mint
import Cardano.Types.PlutusScript (hash) as PlutusScript
import Cardano.Types.Value (add, getMultiAsset, singleton) as Value
import Cardano.Types.Value (lovelaceValueOf)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData (unitDatum)
import Contract.Prelude (bind, discard, mconcat, pure, ($), (/\), (>))
import Contract.ScriptLookups as Lookups
import Contract.Transaction (submitTxFromConstraints)
import Contract.TxConstraints as Constraints
import Dao.Component.Config.Params (mkValidatorConfig)
import Dao.Component.Treasury.Params (TreasuryFundParams)
import Dao.Scripts.Policy (unappliedTreasuryPolicy)
import Dao.Scripts.Validator (unappliedTreasuryValidator)
import Dao.Utils.Contract (ContractResult(ContractResult))
import Dao.Utils.Error (guardContract)
import Dao.Utils.Query (getAllWalletUtxos)
import Data.Array (head)
import Data.Map as Map
import Data.Newtype (unwrap)
import JS.BigInt (fromInt)

-- | Contract for creating token corresponding to the 'voteFungibleCurrencySymbol' field of the config
-- | This token acts as a multiplier of a user's voting weight
createTreasuryFund ::
  TreasuryFundParams ->
  Contract ContractResult
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

  appliedTreasuryPolicy :: PlutusScript <- unappliedTreasuryPolicy txIn
  appliedTreasuryValidator :: PlutusScript <- unappliedTreasuryValidator
    validatorConfig

  let
    treasuryValidatorHash :: ScriptHash
    treasuryValidatorHash = PlutusScript.hash appliedTreasuryValidator

    treasurySymbol :: ScriptHash
    treasurySymbol = PlutusScript.hash appliedTreasuryPolicy

    treasuryValue :: Value
    treasuryValue = Value.singleton treasurySymbol (unwrap adaToken) BigNum.one

  adaAmount <- liftContractM "Could not convert ADA amount to BigNum" $
    BigNum.fromBigInt params.adaAmount

  let
    adaValue :: Value
    adaValue = lovelaceValueOf adaAmount

  treasuryValueWithAda <- liftContractM "Could not build treasuryValueWithAda" $
    Value.add treasuryValue adaValue

  let
    lookups :: Lookups.ScriptLookups
    lookups = mconcat
      [ Lookups.plutusMintingPolicy appliedTreasuryPolicy
      , Lookups.unspentOutputs userUtxos
      ]

    constraints :: Constraints.TxConstraints
    constraints = mconcat
      [ Constraints.mustMintValue $ Mint.fromMultiAsset $ Value.getMultiAsset
          treasuryValue
      , Constraints.mustSpendPubKeyOutput txIn
      , Constraints.mustPayToScript
          treasuryValidatorHash
          unitDatum
          Constraints.DatumInline
          treasuryValueWithAda
      ]

  txHash <- submitTxFromConstraints lookups constraints

  pure $ ContractResult
    { txHash
    , symbol: treasurySymbol
    , tokenName: unwrap adaToken
    }

module Dao.Component.Proposal.Query
  ( QueryResult(..)
  , getTokenNameAndDatumFromOutput
  ) where

import Contract.Prelude

import Cardano.Types (AssetName, ScriptHash, TransactionInput, TransactionOutput(TransactionOutput))
import Cardano.Types.Value (getMultiAsset) as Value
import Dao.Utils.Datum (extractOutputDatum)
import Data.Map (lookup, toUnfoldable) as Map
import Data.Newtype (unwrap)
import LambdaBuffers.ApplicationTypes.Tally (TallyStateDatum)
import Type.Proxy (Proxy(Proxy))

-- | General proposal query result
newtype QueryResult = QueryResult
  { proposalTokenName :: AssetName
  , tallyDatum :: TallyStateDatum
  }

derive instance Newtype QueryResult _

derive newtype instance Show QueryResult

-- | Search for a token name belonging to the given symbol at the output.
-- | Return it along with the datum if found, otherwise return Nothing.
getTokenNameAndDatumFromOutput ::
  ScriptHash ->
  (TransactionInput /\ TransactionOutput) ->
  Maybe QueryResult
getTokenNameAndDatumFromOutput
  symbol
  (_ /\ txOut'@(TransactionOutput txOut)) = do
  tokenNameMap <- Map.lookup symbol $ unwrap $ Value.getMultiAsset txOut.amount
  case Map.toUnfoldable tokenNameMap of
    [ (proposalTokenName /\ _) ] -> do
      tallyDatum <- extractOutputDatum (Proxy :: Proxy TallyStateDatum) txOut'
      Just $ QueryResult { proposalTokenName, tallyDatum }
    _ -> Nothing

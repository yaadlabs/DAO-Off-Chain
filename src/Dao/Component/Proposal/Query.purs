module Dao.Component.Proposal.Query
  ( QueryResult(..)
  , getTokenNameAndDatumFromOutput
  ) where

import Contract.Prelude

import Contract.Transaction
  ( TransactionInput
  , TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  )
import Contract.Value (CurrencySymbol, TokenName, getValue)
import Ctl.Internal.Plutus.Types.AssocMap
  ( Map(Map)
  , lookup
  ) as Plutus.Map
import Dao.Utils.Datum (extractOutputDatum)
import LambdaBuffers.ApplicationTypes.Tally (TallyStateDatum)
import Type.Proxy (Proxy(Proxy))

-- | General proposal query result
newtype QueryResult = QueryResult
  { proposalTokenName :: TokenName
  , tallyDatum :: TallyStateDatum
  }

derive instance Newtype QueryResult _

derive newtype instance Show QueryResult

-- | Search for a token name belonging to the given symbol at the output.
-- | Return it along with the datum if found, otherwise return Nothing.
getTokenNameAndDatumFromOutput ::
  CurrencySymbol ->
  (TransactionInput /\ TransactionOutputWithRefScript) ->
  Maybe QueryResult
getTokenNameAndDatumFromOutput
  symbol
  (_ /\ txOut'@(TransactionOutputWithRefScript txOut)) = do
  tokenNameMap <- Plutus.Map.lookup symbol
    (txOut.output # unwrap # _.amount # getValue)
  case tokenNameMap of
    Plutus.Map.Map [ (proposalTokenName /\ _) ] -> do
      tallyDatum <- extractOutputDatum (Proxy :: Proxy TallyStateDatum) txOut'
      Just $ QueryResult { proposalTokenName, tallyDatum }
    _ -> Nothing

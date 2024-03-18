module Dao.Utils.Datum (extractOutputDatum) where

import Contract.PlutusData
  ( class FromData
  , Datum(Datum)
  , OutputDatum(OutputDatum)
  , fromData
  )
import Contract.Prelude (unwrap, (#))
import Contract.Transaction
  ( TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  )
import Data.Maybe (Maybe(Just, Nothing))
import Type.Proxy (Proxy)

extractOutputDatum ::
  forall (datum' :: Type).
  FromData datum' =>
  Proxy datum' ->
  TransactionOutputWithRefScript ->
  Maybe datum'
extractOutputDatum _ (TransactionOutputWithRefScript txOut) =
  case txOut.output # unwrap # _.datum of
    OutputDatum (Datum rawInlineDatum) -> case fromData rawInlineDatum of
      Just (datum :: datum') -> Just datum
      _ -> Nothing
    _ -> Nothing

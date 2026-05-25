module Dao.Utils.Datum (extractOutputDatum) where

import Cardano.Types (TransactionOutput(TransactionOutput))
import Contract.PlutusData (class FromData, OutputDatum(OutputDatum), fromData)
import Data.Maybe (Maybe(Just, Nothing))
import Type.Proxy (Proxy)

extractOutputDatum ::
  forall (datum' :: Type).
  FromData datum' =>
  Proxy datum' ->
  TransactionOutput ->
  Maybe datum'
extractOutputDatum _ (TransactionOutput txOut) =
  case txOut.datum of
    Just (OutputDatum rawInlineDatum) -> case fromData rawInlineDatum of
      Just (datum :: datum') -> Just datum
      _ -> Nothing
    _ -> Nothing

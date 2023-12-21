{-|
Module: Dao.Utils.Datum
Description: Helpers for dealing with datums
-}
module Dao.Utils.Datum
  ( getInlineDatumFromTxOutWithRefScript
  ) where

import Contract.PlutusData (Datum)
import Contract.Transaction
  ( OutputDatum(OutputDatum)
  , TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  )
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap)

-- | Check that output datum at 'TransactionOutputWithRefScript'
-- contains an inline datum, return if so, otherwise return Nothing
getInlineDatumFromTxOutWithRefScript ::
  TransactionOutputWithRefScript -> Maybe Datum
getInlineDatumFromTxOutWithRefScript
  (TransactionOutputWithRefScript { output }) =
  case (unwrap output).datum of
    OutputDatum datum' -> Just datum'
    _ -> Nothing

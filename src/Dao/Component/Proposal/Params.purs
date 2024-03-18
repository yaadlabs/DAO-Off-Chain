{-|
Module: Dao.Component.Proposal.Params
Description: Helpers for create proposal workflow
-}
module Dao.Component.Proposal.Params
  ( CreateProposalParams(..)
  , QueryProposalParams(..)
  ) where

import Contract.Value (CurrencySymbol, TokenName)
import Data.Newtype (class Newtype)
import LambdaBuffers.ApplicationTypes.Tally (TallyStateDatum)

-- | Create proposal contract paramaters
newtype CreateProposalParams = CreateProposalParams
  { configSymbol :: CurrencySymbol
  , indexSymbol :: CurrencySymbol
  , configTokenName :: TokenName
  , indexTokenName :: TokenName
  , tallyStateDatum :: TallyStateDatum
  }

derive instance Newtype CreateProposalParams _

-- | Query proposal contract paramaters
newtype QueryProposalParams = QueryProposalParams
  { configSymbol :: CurrencySymbol
  , indexSymbol :: CurrencySymbol
  , configTokenName :: TokenName
  , indexTokenName :: TokenName
  }

derive instance Newtype QueryProposalParams _

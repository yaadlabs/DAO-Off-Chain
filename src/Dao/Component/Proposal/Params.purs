{-|
Module: Dao.Component.Proposal.Params
Description: Helpers for create proposal workflow
-}
module Dao.Component.Proposal.Params (CreateProposalParams) where

import Contract.Value (CurrencySymbol, TokenName)

-- | Create proposal contract paramaters
type CreateProposalParams =
  { configSymbol :: CurrencySymbol
  , indexSymbol :: CurrencySymbol
  , configTokenName :: TokenName
  , indexTokenName :: TokenName
  }

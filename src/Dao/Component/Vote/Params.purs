{-|
Module: Dao.Component.Vote.Params
Description: Helpers for vote on proposal workflow
-}
module Dao.Component.Vote.Params (VoteParams) where

import Contract.Value (CurrencySymbol, TokenName)
import JS.BigInt (BigInt)
import LambdaBuffers.ApplicationTypes.Vote (VoteDirection)

-- | Create proposal contract paramaters
type VoteParams =
  { configSymbol :: CurrencySymbol
  , tallySymbol :: CurrencySymbol
  , configTokenName :: TokenName
  , voteSymbol :: CurrencySymbol
  , voteTokenName :: TokenName
  -- Vote datum fields
  , proposalTokenName :: TokenName
  , voteDirection :: VoteDirection
  , returnAda :: BigInt
  }

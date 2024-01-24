{-|
Module: Dao.Component.Vote.Params
Description: Helpers for vote on proposal workflow
-}
module Dao.Component.Vote.Params
  ( VoteOnProposalParams
  , CountVoteParams
  , CancelVoteParams
  ) where

import Contract.Scripts (MintingPolicy)
import Contract.Value (CurrencySymbol, TokenName)
import JS.BigInt (BigInt)
import LambdaBuffers.ApplicationTypes.Vote (VoteDirection)

-- | Create proposal contract paramaters
type VoteOnProposalParams =
  { configSymbol :: CurrencySymbol
  , tallySymbol :: CurrencySymbol
  , configTokenName :: TokenName
  , voteTokenName :: TokenName
  -- Vote NFT symbol (vote pass)
  , voteNftSymbol :: CurrencySymbol
  -- Fungible token symbol (vote multiplier token)
  , fungibleSymbol :: CurrencySymbol
  -- Vote datum fields
  , proposalTokenName :: TokenName
  , voteDirection :: VoteDirection
  , returnAda :: BigInt
  }

-- | Count vote contract paramaters
type CountVoteParams =
  { voteNftSymbol :: CurrencySymbol
  , voteTokenName :: TokenName
  , voteNftTokenName :: TokenName
  , configSymbol :: CurrencySymbol
  , configTokenName :: TokenName
  , tallySymbol :: CurrencySymbol
  , fungibleSymbol :: CurrencySymbol
  , fungibleTokenName :: TokenName
  , fungiblePercent :: BigInt
  }

-- | Cancel vote contract paramaters
type CancelVoteParams =
  { configSymbol :: CurrencySymbol
  , configTokenName :: TokenName
  , voteTokenName :: TokenName
  , voteNftSymbol :: CurrencySymbol
  , voteNftTokenName :: TokenName
  , fungibleSymbol :: CurrencySymbol
  , fungibleTokenName :: TokenName
  }

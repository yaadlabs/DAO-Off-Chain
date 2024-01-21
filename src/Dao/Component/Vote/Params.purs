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
  -- Vote datum fields
  , proposalTokenName :: TokenName
  , voteDirection :: VoteDirection
  , returnAda :: BigInt
  }

-- | Count vote contract paramaters
type CountVoteParams =
  { voteSymbol :: CurrencySymbol
  , voteNftSymbol :: CurrencySymbol
  , voteTokenName :: TokenName
  , voteNftTokenName :: TokenName
  , configSymbol :: CurrencySymbol
  , configTokenName :: TokenName
  , tallySymbol :: CurrencySymbol
  , votePolicy :: MintingPolicy
  }

-- | Cancel vote contract paramaters
type CancelVoteParams =
  { configSymbol :: CurrencySymbol
  , configTokenName :: TokenName
  , voteTokenName :: TokenName
  , voteNftSymbol :: CurrencySymbol
  , voteNftTokenName :: TokenName
  }

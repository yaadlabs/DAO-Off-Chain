{-|
Module: Dao.Component.Vote.Params
Description: Helpers for vote on proposal workflow
-}
module Dao.Component.Vote.Params
  ( VoteOnProposalParams(..)
  , CountVoteParams(..)
  , CancelVoteParams(..)
  ) where

import Contract.Scripts (MintingPolicy)
import Contract.Value (CurrencySymbol, TokenName)
import Data.Newtype (class Newtype)
import JS.BigInt (BigInt)
import LambdaBuffers.ApplicationTypes.Vote (VoteDirection)

-- | Create proposal contract paramaters
newtype VoteOnProposalParams = VoteOnProposalParams
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

derive instance Newtype VoteOnProposalParams _

-- | Count vote contract paramaters
newtype CountVoteParams = CountVoteParams
  { voteSymbol :: CurrencySymbol
  , voteNftSymbol :: CurrencySymbol
  , voteTokenName :: TokenName
  , voteNftTokenName :: TokenName
  , configSymbol :: CurrencySymbol
  , configTokenName :: TokenName
  , tallySymbol :: CurrencySymbol
  }

derive instance Newtype CountVoteParams _

-- | Cancel vote contract paramaters
newtype CancelVoteParams = CancelVoteParams
  { configSymbol :: CurrencySymbol
  , configTokenName :: TokenName
  , voteTokenName :: TokenName
  }

derive instance Newtype CancelVoteParams _

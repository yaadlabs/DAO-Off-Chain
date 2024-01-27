{-|
Module: Dao.Component.Vote.Params
Description: Helpers for vote on proposal workflow
-}
module Dao.Component.Vote.Params
  ( VoteOnProposalParams(..)
  , CountVoteParams(..)
  , CancelVoteParams(..)
  ) where

import Contract.Address (Address)
import Contract.Scripts (MintingPolicy)
import Contract.Value (CurrencySymbol, TokenName)
import Data.Newtype (class Newtype)
import JS.BigInt (BigInt)
import LambdaBuffers.ApplicationTypes.Vote (VoteDirection)

-- | Create proposal contract paramaters
newtype VoteOnProposalParams = VoteOnProposalParams
  { configSymbol :: CurrencySymbol
  , configTokenName :: TokenName
  , tallySymbol :: CurrencySymbol
  -- Vote datum fields
  , proposalTokenName :: TokenName
  , voteDirection :: VoteDirection
  , returnAda :: BigInt
  }

derive instance Newtype VoteOnProposalParams _

-- | Count vote contract paramaters
newtype CountVoteParams = CountVoteParams
  { configSymbol :: CurrencySymbol
  , configTokenName :: TokenName
  , tallySymbol :: CurrencySymbol
  , voteTokenName :: TokenName
  }

derive instance Newtype CountVoteParams _

-- | Cancel vote contract paramaters
newtype CancelVoteParams = CancelVoteParams
  { configSymbol :: CurrencySymbol
  , configTokenName :: TokenName
  }

derive instance Newtype CancelVoteParams _

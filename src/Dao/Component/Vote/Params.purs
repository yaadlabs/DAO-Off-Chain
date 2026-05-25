{-|
Module: Dao.Component.Vote.Params
Description: Helpers for voting related contracts
-}
module Dao.Component.Vote.Params
  ( VoteOnProposalParams(..)
  , CountVoteParams(..)
  , CancelVoteParams(..)
  ) where

import Cardano.Types (AssetName, ScriptHash)
import Data.Newtype (class Newtype)
import JS.BigInt (BigInt)
import LambdaBuffers.ApplicationTypes.Vote (VoteDirection)

-- | Create proposal contract paramaters
newtype VoteOnProposalParams = VoteOnProposalParams
  { configSymbol :: ScriptHash
  , configTokenName :: AssetName
  , tallySymbol :: ScriptHash
  -- Vote datum fields
  , proposalTokenName :: AssetName
  , voteDirection :: VoteDirection
  , returnAda :: BigInt
  }

derive instance Newtype VoteOnProposalParams _

-- | Count vote contract paramaters
newtype CountVoteParams = CountVoteParams
  { configSymbol :: ScriptHash
  , configTokenName :: AssetName
  , tallySymbol :: ScriptHash
  , proposalTokenName :: AssetName
  }

derive instance Newtype CountVoteParams _

-- | Cancel vote contract paramaters
newtype CancelVoteParams = CancelVoteParams
  { configSymbol :: ScriptHash
  , configTokenName :: AssetName
  , proposalTokenName :: AssetName
  }

derive instance Newtype CancelVoteParams _

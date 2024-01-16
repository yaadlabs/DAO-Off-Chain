module Dao.Web.Types where

import Contract.Prelude

import Aeson as Aeson
import JS.BigInt (BigInt)

-- | TokenName represented as a wrapped String
newtype TokenName = TokenName String

instance Show TokenName where
  show (TokenName tn) = show tn

derive newtype instance Aeson.DecodeAeson TokenName

-- | 56-character long hex string
-- | Used to represent a 'CurrencySymbol'
newtype Hash28 = Hash28 String

derive instance Newtype Hash28 _

instance Show Hash28 where
  show (Hash28 h) = h

derive newtype instance Aeson.DecodeAeson Hash28

-- | ScriptHash represented as a wrapped String
newtype ScriptHash = ScriptHash String

-- | POSIXTime represented as a wrapped BigInt
newtype POSIXTime = POSIXTime BigInt

-- | Address represented as a wrapped String
newtype Address = Address String

instance Show Address where
  show (Address a) = a

-- | Parameters passed when initially creating dynamic config
newtype CreateConfigParams = CreateConfigParams
  { configTokenName :: TokenName
  , upgradeMajorityPercent :: BigInt
  , upgradeRelativeMajorityPercent :: BigInt
  , generalMajorityPercent :: BigInt
  , generalRelativeMajorityPercent :: BigInt
  , tripMajorityPercent :: BigInt
  , tripRelativeMajorityPercent :: BigInt
  , totalVotes :: BigInt
  , maxGeneralDisbursement :: BigInt
  , maxTripDisbursement :: BigInt
  , agentDisbursementPercent :: BigInt
  , proposalTallyEndOffset :: BigInt
  , tallyNft :: Hash28
  , voteCurrencySymbol :: Hash28
  , voteTokenName :: TokenName
  , voteNft :: Hash28
  , voteFungibleCurrencySymbol :: Hash28
  , voteFungibleTokenName :: TokenName
  , fungibleVotePercent :: BigInt
  }

-- | Parameters passed for the upgrade config proposal contract
newtype UpgradeConfigParams = UpgradeConfigParams
  { newDynamicConfigDatum :: DynamicConfigDatum
  , configSymbol :: Hash28
  , configTokenName :: TokenName
  , tallySymbol :: Hash28
  }

newtype DynamicConfigDatum = DynamicConfigDatum
  { tallyValidator :: ScriptHash
  , treasuryValidator :: ScriptHash
  , configurationValidator :: ScriptHash
  , voteValidator :: ScriptHash
  , upgradeMajorityPercent :: BigInt
  , upgradeRelativeMajorityPercent :: BigInt
  , generalMajorityPercent :: BigInt
  , generalRelativeMajorityPercent :: BigInt
  , tripMajorityPercent :: BigInt
  , tripRelativeMajorityPercent :: BigInt
  , totalVotes :: BigInt
  , maxGeneralDisbursement :: BigInt
  , maxTripDisbursement :: BigInt
  , agentDisbursementPercent :: BigInt
  , proposalTallyEndOffset :: BigInt
  , tallyNft :: Hash28
  , voteCurrencySymbol :: Hash28
  , voteTokenName :: TokenName
  , voteNft :: Hash28
  , voteFungibleCurrencySymbol :: Hash28
  , voteFungibleTokenName :: TokenName
  , fungibleVotePercent :: BigInt
  }

-- | Create proposal contract paramaters
newtype CreateProposalParams = CreateProposalParams
  { configSymbol :: Hash28
  , indexSymbol :: Hash28
  , configTokenName :: TokenName
  , indexTokenName :: TokenName
  , tallyStateDatum :: TallyStateDatum
  }

newtype TallyStateDatum = TallyStateDatum
  { proposal :: ProposalType
  , proposalEndTime :: POSIXTime
  , for :: BigInt
  , against :: BigInt
  }

data ProposalType
  = ProposalType'Upgrade Hash28
  | ProposalType'General Address BigInt
  | ProposalType'Trip Address Address BigInt

-- | Parameters for treasury trip contract
newtype TreasuryTripParams = TreasuryTripParams
  { travelAgentAddress :: Address
  , travellerAddress :: Address
  , totalTravelCost :: BigInt
  , configSymbol :: Hash28
  , configTokenName :: TokenName
  , tallySymbol :: Hash28
  , treasurySymbol :: Hash28
  }

-- | Parameters for treasury general contract
newtype TreasuryGeneralParams = TreasuryGeneralParams
  { paymentAddress :: Address
  , generalPaymentAmount :: BigInt
  , configSymbol :: Hash28
  , tallySymbol :: Hash28
  , treasurySymbol :: Hash28
  , configTokenName :: TokenName
  }

-- | Create proposal contract paramaters
newtype VoteOnProposalParams = VoteOnProposalParams
  { configSymbol :: Hash28
  , tallySymbol :: Hash28
  , configTokenName :: TokenName
  , voteTokenName :: TokenName
  -- Vote NFT symbol (vote pass)
  , voteNftSymbol :: Hash28
  -- Vote datum fields
  , proposalTokenName :: TokenName
  , voteDirection :: VoteDirection
  , returnAda :: BigInt
  }

data VoteDirection = VoteDirection'For | VoteDirection'Against

-- | Count vote contract paramaters
newtype CountVoteParams = CountVoteParams
  { voteSymbol :: Hash28
  , voteNftSymbol :: Hash28
  , voteTokenName :: TokenName
  , voteNftTokenName :: TokenName
  , configSymbol :: Hash28
  , configTokenName :: TokenName
  , tallySymbol :: Hash28
  }

-- | Cancel vote contract paramaters
newtype CancelVoteParams = CancelVoteParams
  { configSymbol :: Hash28
  , configTokenName :: TokenName
  , voteTokenName :: TokenName
  }

module Dao.Web.Types where

import Aeson as Aeson
import Contract.Time (POSIXTime)
import Data.Newtype (class Newtype)
import Data.Show (class Show, show)
import JS.BigInt (BigInt)

-- * Non app-specific / CTL types

-- | TokenName represented as a wrapped String
newtype TokenName = TokenName String

instance Show TokenName where
  show (TokenName tn) = show tn

derive newtype instance Aeson.DecodeAeson TokenName

-- | 56-character hex string
-- | Used to represent a 'CurrencySymbol'
newtype Hash28 = Hash28 String

derive instance Newtype Hash28 _

instance Show Hash28 where
  show (Hash28 h) = h

derive newtype instance Aeson.DecodeAeson Hash28

-- | 64-character hex string
-- | Used to represent a TransactionHash
newtype Hash32 = Hash32 String

derive instance Newtype Hash32 _

instance Show Hash32 where
  show (Hash32 h) = h

derive newtype instance Aeson.DecodeAeson Hash32

-- | ScriptHash represented as a wrapped String
newtype ScriptHash = ScriptHash String

-- | PaymentPubKeyHash represented as a wrapped String
newtype PaymentPubKeyHash = PaymentPubKeyHash String

-- | TransactionHash represented as a 64-character String
type TransactionHash = Hash32

-- | Address represented as a wrapped String
newtype Address = Address String

instance Show Address where
  show (Address a) = a

-- * Contract parameters

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
  , voteTokenName :: TokenName
  , voteNftSymbol :: Hash28
  , voteFungibleCurrencySymbol :: Hash28
  , voteFungibleTokenName :: TokenName
  , fungibleVotePercent :: BigInt
  , indexSymbol :: Hash28
  , indexTokenName :: TokenName
  }

-- | Parameters passed for the upgrade config proposal contract
newtype UpgradeConfigParams = UpgradeConfigParams
  { newDynamicConfigDatum :: DynamicConfigDatum
  , configSymbol :: Hash28
  , configTokenName :: TokenName
  , tallySymbol :: Hash28
  }

-- | Create proposal contract paramaters
newtype CreateProposalParams = CreateProposalParams
  { configSymbol :: Hash28
  , indexSymbol :: Hash28
  , configTokenName :: TokenName
  , indexTokenName :: TokenName
  , tallyStateDatum :: TallyStateDatum
  }

-- | Parameters for treasury general contract
newtype TreasuryParams = TreasuryParams
  { configSymbol :: Hash28
  , tallySymbol :: Hash28
  , treasurySymbol :: Hash28
  , configTokenName :: TokenName
  }

-- | Create proposal contract paramaters
newtype VoteOnProposalParams = VoteOnProposalParams
  { configSymbol :: Hash28
  , configTokenName :: TokenName
  , tallySymbol :: Hash28
  -- Vote datum fields
  , proposalTokenName :: TokenName
  , voteDirection :: VoteDirection
  , returnAda :: BigInt
  }

-- | Count vote contract paramaters
newtype CountVoteParams = CountVoteParams
  { configSymbol :: Hash28
  , configTokenName :: TokenName
  , tallySymbol :: Hash28
  , proposalTokenName :: TokenName
  , voteTokenName :: TokenName
  }

-- | Cancel vote contract paramaters
newtype CancelVoteParams = CancelVoteParams
  { configSymbol :: Hash28
  , configTokenName :: TokenName
  , proposalTokenName :: TokenName
  }

-- | Create fungible contract paramaters
newtype CreateFungibleParams = CreateFungibleParams
  { userPkh :: PaymentPubKeyHash
  , amount :: BigInt
  }

-- * Contract Results

-- | ContractResult
newtype ContractResult = ContractResult
  { txHash :: TransactionHash
  , symbol :: Hash28
  , tokenName :: TokenName
  }

-- | VoteOnProposalResult
newtype VoteOnProposalResult = VoteOnProposalResult
  { txHash :: TransactionHash
  , symbol :: Hash28
  }

-- * Datums

-- | The app's config datum
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

-- | Proposal datum
newtype TallyStateDatum = TallyStateDatum
  { proposal :: ProposalType
  , proposalEndTime :: POSIXTime
  , for :: BigInt
  , against :: BigInt
  }

-- | Vote datum
newtype VoteDatum = VoteDatum
  { proposalTokenName :: TokenName
  , direction :: VoteDirection
  , voteOwner :: Address
  , returnAda :: BigInt
  }

-- | Index datum
newtype IndexNftDatum = IndexNftDatum { index :: BigInt }

data VoteDirection = For | Against

data ProposalType
  = Upgrade Hash28
  | General Address BigInt
  | Trip Address Address BigInt

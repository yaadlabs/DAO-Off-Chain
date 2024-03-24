module Dao.Web.Types where

import Contract.Prelude

import Aeson as Aeson
import Contract.Time (POSIXTime)
import Data.Newtype (class Newtype)
import Data.Show (class Show, show)
import Foreign (Foreign)
import Foreign (unsafeFromForeign, unsafeToForeign) as Foreign
import JS.BigInt (BigInt)

-- * Non app-specific / CTL types

-- | Simplified CTL config (to be expanded)
newtype CtlConfig = CtlConfig
  { blockfrostApiKey :: String
  , network :: String
  } 

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

-- | JS Maybe type

newtype JsMaybe :: forall k. k -> Type
newtype JsMaybe a = JsMaybe Foreign

instance Show a => Show (JsMaybe a) where
  show x = case fromJsMaybe x of
    Nothing -> "null"
    Just x' -> show x'

fromJsMaybe :: forall a. JsMaybe a -> Maybe a
fromJsMaybe (JsMaybe x) =
  if isNullOrUndefined x then Nothing else Just (Foreign.unsafeFromForeign x)

foreign import isNullOrUndefined :: Foreign -> Boolean
foreign import nullF :: Foreign

toJsMaybe :: forall a. Maybe a -> JsMaybe a
toJsMaybe = JsMaybe <<< maybe nullF Foreign.unsafeToForeign

toJsMaybe' :: forall a. a -> JsMaybe a
toJsMaybe' = JsMaybe <<< Foreign.unsafeToForeign

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
  , proposalTokenName :: TokenName
  }

-- | Create proposal contract paramaters
newtype CreateProposalParams = CreateProposalParams
  { configSymbol :: Hash28
  , indexSymbol :: Hash28
  , configTokenName :: TokenName
  , indexTokenName :: TokenName
  , tallyStateDatum :: TallyStateDatum
  }

-- | Query proposal contract paramaters
newtype QueryProposalParams = QueryProposalParams
  { configSymbol :: Hash28
  , indexSymbol :: Hash28
  , configTokenName :: TokenName
  , indexTokenName :: TokenName
  }

-- | Parameters for treasury general contract
newtype TreasuryParams = TreasuryParams
  { configSymbol :: Hash28
  , tallySymbol :: Hash28
  , proposalTokenName :: TokenName
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

-- | CreateConfigResult
newtype CreateConfigResult = CreateConfigResult
  { txHash :: TransactionHash
  , symbol :: Hash28
  , tokenName :: TokenName
  , tallySymbol :: Hash28
  }

-- | VoteOnProposalResult
newtype VoteOnProposalResult = VoteOnProposalResult
  { txHash :: TransactionHash
  , symbol :: Hash28
  }

-- | General proposal query result
newtype QueryResult = QueryResult
  { proposalTokenName :: TokenName
  , tallyDatum :: TallyStateDatum
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
newtype IndexDatum = IndexDatum { index :: BigInt }

data VoteDirection = For | Against

data ProposalType
  = Upgrade Hash28
  | General Address BigInt
  | Trip Address Address BigInt

{-|
Module: Dao.Web.Conversion
Description: For conversions between JavaScript and PureScript
-}
module Dao.Web.Conversion where

import Contract.Address
  ( Address
  , AddressWithNetworkTag(AddressWithNetworkTag)
  , PaymentPubKeyHash(PaymentPubKeyHash)
  , PubKeyHash(PubKeyHash)
  , addressWithNetworkTagFromBech32
  , addressWithNetworkTagToBech32
  ) as Ctl
import Contract.Config
  ( NetworkId
  ) as Ctl
import Contract.Prelude
  ( bind
  , pure
  , show
  , unwrap
  , ($)
  , (<<<)
  , (<>)
  , (>>=)
  )
import Contract.Prim.ByteArray (byteArrayToHex, hexToByteArray) as Ctl
import Contract.Prim.ByteArray (rawBytesToHex)
import Contract.Transaction (TransactionHash(TransactionHash)) as Ctl
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , getCurrencySymbol
  , getTokenName
  , mkCurrencySymbol
  , mkTokenName
  ) as Ctl
import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
import Ctl.Internal.Serialization.Hash (ScriptHash) as Ctl
import Ctl.Internal.Serialization.Hash
  ( ed25519KeyHashFromBytes
  , ed25519KeyHashToBytes
  , scriptHashFromBytes
  , scriptHashToBytes
  )
import Dao.Component.Config.Params
  ( CreateConfigParams(CreateConfigParams)
  , UpgradeConfigParams(UpgradeConfigParams)
  ) as DaoApi
import Dao.Component.Fungible.Params
  ( CreateFungibleParams(CreateFungibleParams)
  ) as DaoApi
import Dao.Component.Proposal.Params
  ( CreateProposalParams(CreateProposalParams)
  ) as DaoApi
import Dao.Component.Treasury.Params
  ( TreasuryParams(TreasuryParams)
  ) as DaoApi
import Dao.Component.Vote.Params
  ( CancelVoteParams(CancelVoteParams)
  , CountVoteParams(CountVoteParams)
  , VoteOnProposalParams(VoteOnProposalParams)
  ) as DaoApi
import Dao.Utils.Contract (ContractResult(ContractResult)) as DaoApi
import Dao.Web.Types as WebApi
import Dao.Workflow.VoteOnProposal (VoteOnProposalResult(VoteOnProposalResult)) as DaoApi
import Data.Either (Either(Left))
import Data.Maybe (Maybe(Just, Nothing))
import LambdaBuffers.ApplicationTypes.Configuration
  ( DynamicConfigDatum(DynamicConfigDatum)
  ) as DaoApi
import LambdaBuffers.ApplicationTypes.Proposal
  ( ProposalType(ProposalType'General, ProposalType'Trip, ProposalType'Upgrade)
  ) as DaoApi
import LambdaBuffers.ApplicationTypes.Tally (TallyStateDatum(TallyStateDatum)) as DaoApi
import LambdaBuffers.ApplicationTypes.Vote
  ( VoteDirection(VoteDirection'For, VoteDirection'Against)
  ) as DaoApi

type Conversion a = ReaderT Ctl.NetworkId (Either String) a

class ConvertPsToJs js ps | js -> ps where
  convertPsToJs :: ps -> Conversion js

class (ConvertPsToJs js ps) <= ConvertJsToPs js ps | js -> ps where
  convertJsToPs :: js -> Conversion ps

runConvertJsToPs ::
  forall js ps.
  ConvertJsToPs js ps =>
  js ->
  Ctl.NetworkId ->
  Either String ps
runConvertJsToPs js = runReaderT $ convertJsToPs js

runConvertPsToJs ::
  forall js ps.
  ConvertPsToJs js ps =>
  ps ->
  Ctl.NetworkId ->
  Either String js
runConvertPsToJs ps = runReaderT $ convertPsToJs ps

note :: forall a. String -> Maybe a -> Conversion a
note _ (Just a) = pure a
note msg Nothing = lift $ Left msg

-- * DynamicConfigDatum

instance ConvertPsToJs WebApi.DynamicConfigDatum DaoApi.DynamicConfigDatum where
  convertPsToJs (DaoApi.DynamicConfigDatum params) = do

    tallyValidator' <- convertPsToJs params.tallyValidator
    treasuryValidator' <- convertPsToJs params.treasuryValidator
    configValidator' <- convertPsToJs params.configurationValidator
    voteValidator' <- convertPsToJs params.voteValidator
    tallyNft' <- convertPsToJs params.tallyNft
    voteCurrencySymbol' <- convertPsToJs params.voteCurrencySymbol
    voteTokenName' <- convertPsToJs params.voteTokenName
    voteNft' <- convertPsToJs params.voteNft
    voteFungibleCurrencySymbol' <- convertPsToJs
      params.voteFungibleCurrencySymbol
    voteFungibleTokenName' <- convertPsToJs params.voteFungibleTokenName

    pure $ WebApi.DynamicConfigDatum
      { tallyValidator: tallyValidator'
      , treasuryValidator: treasuryValidator'
      , configurationValidator: configValidator'
      , voteValidator: voteValidator'
      , upgradeMajorityPercent: params.upgradeMajorityPercent
      , upgradeRelativeMajorityPercent: params.upgradeRelativeMajorityPercent
      , generalMajorityPercent: params.generalMajorityPercent
      , generalRelativeMajorityPercent: params.generalRelativeMajorityPercent
      , tripMajorityPercent: params.tripMajorityPercent
      , tripRelativeMajorityPercent: params.tripRelativeMajorityPercent
      , totalVotes: params.totalVotes
      , maxGeneralDisbursement: params.maxGeneralDisbursement
      , maxTripDisbursement: params.maxTripDisbursement
      , agentDisbursementPercent: params.agentDisbursementPercent
      , proposalTallyEndOffset: params.proposalTallyEndOffset
      , tallyNft: tallyNft'
      , voteCurrencySymbol: voteCurrencySymbol'
      , voteTokenName: voteTokenName'
      , voteNft: voteNft'
      , voteFungibleCurrencySymbol: voteFungibleCurrencySymbol'
      , voteFungibleTokenName: voteFungibleTokenName'
      , fungibleVotePercent: params.fungibleVotePercent
      }

instance ConvertJsToPs WebApi.DynamicConfigDatum DaoApi.DynamicConfigDatum where
  convertJsToPs (WebApi.DynamicConfigDatum params) = do

    tallyValidator' <- convertJsToPs params.tallyValidator
    treasuryValidator' <- convertJsToPs params.treasuryValidator
    configValidator' <- convertJsToPs params.configurationValidator
    voteValidator' <- convertJsToPs params.voteValidator
    tallyNft' <- convertJsToPs params.tallyNft
    voteCurrencySymbol' <- convertJsToPs params.voteCurrencySymbol
    voteTokenName' <- convertJsToPs params.voteTokenName
    voteNft' <- convertJsToPs params.voteNft
    voteFungibleCurrencySymbol' <- convertJsToPs
      params.voteFungibleCurrencySymbol
    voteFungibleTokenName' <- convertJsToPs params.voteFungibleTokenName

    pure $ DaoApi.DynamicConfigDatum
      { tallyValidator: tallyValidator'
      , treasuryValidator: treasuryValidator'
      , configurationValidator: configValidator'
      , voteValidator: voteValidator'
      , upgradeMajorityPercent: params.upgradeMajorityPercent
      , upgradeRelativeMajorityPercent: params.upgradeRelativeMajorityPercent
      , generalMajorityPercent: params.generalMajorityPercent
      , generalRelativeMajorityPercent: params.generalRelativeMajorityPercent
      , tripMajorityPercent: params.tripMajorityPercent
      , tripRelativeMajorityPercent: params.tripRelativeMajorityPercent
      , totalVotes: params.totalVotes
      , maxGeneralDisbursement: params.maxGeneralDisbursement
      , maxTripDisbursement: params.maxTripDisbursement
      , agentDisbursementPercent: params.agentDisbursementPercent
      , proposalTallyEndOffset: params.proposalTallyEndOffset
      , tallyNft: tallyNft'
      , voteCurrencySymbol: voteCurrencySymbol'
      , voteTokenName: voteTokenName'
      , voteNft: voteNft'
      , voteFungibleCurrencySymbol: voteFungibleCurrencySymbol'
      , voteFungibleTokenName: voteFungibleTokenName'
      , fungibleVotePercent: params.fungibleVotePercent
      }

-- * ContractResult

instance ConvertPsToJs WebApi.ContractResult DaoApi.ContractResult where
  convertPsToJs (DaoApi.ContractResult params) = do
    txHash <- convertPsToJs params.txHash
    symbol <- convertPsToJs params.symbol
    tokenName <- convertPsToJs params.tokenName

    pure $ WebApi.ContractResult
      { txHash
      , symbol
      , tokenName
      }

instance ConvertJsToPs WebApi.ContractResult DaoApi.ContractResult where
  convertJsToPs (WebApi.ContractResult params) = do
    txHash <- convertJsToPs params.txHash
    symbol <- convertJsToPs params.symbol
    tokenName <- convertJsToPs params.tokenName

    pure $ DaoApi.ContractResult
      { txHash
      , symbol
      , tokenName
      }

-- * VoteOnProposalResult

instance ConvertPsToJs WebApi.VoteOnProposalResult DaoApi.VoteOnProposalResult where
  convertPsToJs (DaoApi.VoteOnProposalResult params) = do
    txHash <- convertPsToJs params.txHash
    symbol <- convertPsToJs params.symbol

    pure $ WebApi.VoteOnProposalResult
      { txHash
      , symbol
      }

instance ConvertJsToPs WebApi.VoteOnProposalResult DaoApi.VoteOnProposalResult where
  convertJsToPs (WebApi.VoteOnProposalResult params) = do
    txHash <- convertJsToPs params.txHash
    symbol <- convertJsToPs params.symbol

    pure $ DaoApi.VoteOnProposalResult
      { txHash
      , symbol
      }

-- * UpgradeConfigParams

instance ConvertPsToJs WebApi.UpgradeConfigParams DaoApi.UpgradeConfigParams where
  convertPsToJs (DaoApi.UpgradeConfigParams params) = do

    newDynamicConfigDatum' <- convertPsToJs params.newDynamicConfigDatum
    configSymbol' <- convertPsToJs params.configSymbol
    configTokenName' <- convertPsToJs params.configTokenName
    tallySymbol' <- convertPsToJs params.tallySymbol

    pure $ WebApi.UpgradeConfigParams
      { newDynamicConfigDatum: newDynamicConfigDatum'
      , configSymbol: configSymbol'
      , configTokenName: configTokenName'
      , tallySymbol: tallySymbol'
      }

instance ConvertJsToPs WebApi.UpgradeConfigParams DaoApi.UpgradeConfigParams where
  convertJsToPs (WebApi.UpgradeConfigParams params) = do

    newDynamicConfigDatum' <- convertJsToPs params.newDynamicConfigDatum
    configSymbol' <- convertJsToPs params.configSymbol
    configTokenName' <- convertJsToPs params.configTokenName
    tallySymbol' <- convertJsToPs params.tallySymbol

    pure $ DaoApi.UpgradeConfigParams
      { newDynamicConfigDatum: newDynamicConfigDatum'
      , configSymbol: configSymbol'
      , configTokenName: configTokenName'
      , tallySymbol: tallySymbol'
      }

-- * TreasuryParams

instance ConvertPsToJs WebApi.TreasuryParams DaoApi.TreasuryParams where
  convertPsToJs (DaoApi.TreasuryParams params) = do

    configSymbol <- convertPsToJs params.configSymbol
    configTokenName <- convertPsToJs params.configTokenName
    tallySymbol <- convertPsToJs params.tallySymbol
    treasurySymbol <- convertPsToJs params.treasurySymbol

    pure $ WebApi.TreasuryParams
      { configSymbol
      , configTokenName
      , tallySymbol
      , treasurySymbol
      }

instance ConvertJsToPs WebApi.TreasuryParams DaoApi.TreasuryParams where
  convertJsToPs (WebApi.TreasuryParams params) = do

    configSymbol <- convertJsToPs params.configSymbol
    tallySymbol <- convertJsToPs params.tallySymbol
    treasurySymbol <- convertJsToPs params.treasurySymbol
    configTokenName <- convertJsToPs params.configTokenName

    pure $ DaoApi.TreasuryParams
      { configSymbol
      , configTokenName
      , tallySymbol
      , treasurySymbol
      }

-- * CreateFungibleParams

instance ConvertPsToJs WebApi.CreateFungibleParams DaoApi.CreateFungibleParams where
  convertPsToJs (DaoApi.CreateFungibleParams params) = do

    userPkh <- convertPsToJs params.userPkh

    pure $ WebApi.CreateFungibleParams
      { userPkh
      , amount: params.amount
      }

instance ConvertJsToPs WebApi.CreateFungibleParams DaoApi.CreateFungibleParams where
  convertJsToPs (WebApi.CreateFungibleParams params) = do

    userPkh <- convertJsToPs params.userPkh

    pure $ DaoApi.CreateFungibleParams
      { userPkh
      , amount: params.amount
      }

-- * CreateConfigParams

instance
  ConvertPsToJs WebApi.CreateConfigParams DaoApi.CreateConfigParams where
  convertPsToJs (DaoApi.CreateConfigParams params) = do

    configTokenName <- convertPsToJs params.configTokenName
    tallyNft <- convertPsToJs params.tallyNft
    voteTokenName <- convertPsToJs params.voteTokenName
    voteFungibleCurrencySymbol <- convertPsToJs
      params.voteFungibleCurrencySymbol
    voteNftSymbol <- convertPsToJs params.voteNftSymbol
    voteFungibleTokenName <- convertPsToJs params.voteFungibleTokenName
    indexSymbol <- convertPsToJs params.indexSymbol
    indexTokenName <- convertPsToJs params.indexTokenName

    pure $ WebApi.CreateConfigParams
      { configTokenName
      , upgradeMajorityPercent: params.upgradeMajorityPercent
      , upgradeRelativeMajorityPercent: params.upgradeRelativeMajorityPercent
      , generalMajorityPercent: params.generalMajorityPercent
      , generalRelativeMajorityPercent: params.generalRelativeMajorityPercent
      , tripMajorityPercent: params.tripMajorityPercent
      , tripRelativeMajorityPercent: params.tripRelativeMajorityPercent
      , totalVotes: params.totalVotes
      , maxGeneralDisbursement: params.maxGeneralDisbursement
      , maxTripDisbursement: params.maxTripDisbursement
      , agentDisbursementPercent: params.agentDisbursementPercent
      , proposalTallyEndOffset: params.proposalTallyEndOffset
      , tallyNft
      , voteTokenName
      , voteNftSymbol
      , voteFungibleCurrencySymbol
      , voteFungibleTokenName
      , fungibleVotePercent: params.fungibleVotePercent
      , indexSymbol
      , indexTokenName
      }

instance
  ConvertJsToPs WebApi.CreateConfigParams DaoApi.CreateConfigParams where
  convertJsToPs (WebApi.CreateConfigParams params) = do

    configTokenName <- convertJsToPs params.configTokenName
    tallyNft <- convertJsToPs params.tallyNft
    voteTokenName <- convertJsToPs params.voteTokenName
    voteNftSymbol <- convertJsToPs params.voteNftSymbol
    voteFungibleCurrencySymbol <- convertJsToPs
      params.voteFungibleCurrencySymbol
    voteFungibleTokenName <- convertJsToPs params.voteFungibleTokenName
    indexSymbol <- convertJsToPs params.indexSymbol
    indexTokenName <- convertJsToPs params.indexTokenName

    pure $ DaoApi.CreateConfigParams
      { configTokenName
      , upgradeMajorityPercent: params.upgradeMajorityPercent
      , upgradeRelativeMajorityPercent: params.upgradeRelativeMajorityPercent
      , generalMajorityPercent: params.generalMajorityPercent
      , generalRelativeMajorityPercent: params.generalRelativeMajorityPercent
      , tripMajorityPercent: params.tripMajorityPercent
      , tripRelativeMajorityPercent: params.tripRelativeMajorityPercent
      , totalVotes: params.totalVotes
      , maxGeneralDisbursement: params.maxGeneralDisbursement
      , maxTripDisbursement: params.maxTripDisbursement
      , agentDisbursementPercent: params.agentDisbursementPercent
      , proposalTallyEndOffset: params.proposalTallyEndOffset
      , tallyNft
      , voteTokenName
      , voteNftSymbol
      , voteFungibleCurrencySymbol
      , voteFungibleTokenName
      , fungibleVotePercent: params.fungibleVotePercent
      , indexSymbol
      , indexTokenName
      }

-- * CreateProposalParams

instance ConvertPsToJs WebApi.CreateProposalParams DaoApi.CreateProposalParams where
  convertPsToJs (DaoApi.CreateProposalParams params) = do

    configSymbol' <- convertPsToJs params.configSymbol
    configTokenName' <- convertPsToJs params.configTokenName
    indexSymbol' <- convertPsToJs params.indexSymbol
    indexTokenName' <- convertPsToJs params.indexTokenName
    tallyStateDatum' <- convertPsToJs params.tallyStateDatum

    pure $ WebApi.CreateProposalParams
      { configSymbol: configSymbol'
      , indexSymbol: indexSymbol'
      , configTokenName: configTokenName'
      , indexTokenName: indexTokenName'
      , tallyStateDatum: tallyStateDatum'
      }

instance ConvertJsToPs WebApi.CreateProposalParams DaoApi.CreateProposalParams where
  convertJsToPs (WebApi.CreateProposalParams params) = do

    configSymbol' <- convertJsToPs params.configSymbol
    configTokenName' <- convertJsToPs params.configTokenName
    indexSymbol' <- convertJsToPs params.indexSymbol
    indexTokenName' <- convertJsToPs params.indexTokenName
    tallyStateDatum' <- convertJsToPs params.tallyStateDatum

    pure $ DaoApi.CreateProposalParams
      { configSymbol: configSymbol'
      , indexSymbol: indexSymbol'
      , configTokenName: configTokenName'
      , indexTokenName: indexTokenName'
      , tallyStateDatum: tallyStateDatum'
      }

-- * CountVoteParams

instance ConvertPsToJs WebApi.CountVoteParams DaoApi.CountVoteParams where
  convertPsToJs (DaoApi.CountVoteParams params) = do

    configSymbol <- convertPsToJs params.configSymbol
    configTokenName <- convertPsToJs params.configTokenName
    tallySymbol <- convertPsToJs params.tallySymbol
    voteTokenName <- convertPsToJs params.voteTokenName

    pure $ WebApi.CountVoteParams
      { configSymbol
      , configTokenName
      , tallySymbol
      , voteTokenName
      }

instance ConvertJsToPs WebApi.CountVoteParams DaoApi.CountVoteParams where
  convertJsToPs (WebApi.CountVoteParams params) = do

    configSymbol <- convertJsToPs params.configSymbol
    configTokenName <- convertJsToPs params.configTokenName
    voteTokenName <- convertJsToPs params.voteTokenName
    tallySymbol <- convertJsToPs params.tallySymbol

    pure $ DaoApi.CountVoteParams
      { configSymbol
      , configTokenName
      , voteTokenName
      , tallySymbol
      }

-- * CancelVoteParams

instance ConvertPsToJs WebApi.CancelVoteParams DaoApi.CancelVoteParams where
  convertPsToJs (DaoApi.CancelVoteParams params) = do

    configSymbol <- convertPsToJs params.configSymbol
    configTokenName <- convertPsToJs params.configTokenName
    voteTokenName <- convertPsToJs params.voteTokenName
    voteNftSymbol <- convertPsToJs params.voteNftSymbol
    voteNftTokenName <- convertPsToJs params.voteNftTokenName
    fungibleSymbol <- convertPsToJs params.fungibleSymbol
    fungibleTokenName <- convertPsToJs params.fungibleTokenName

    pure $ WebApi.CancelVoteParams
      { configSymbol
      , configTokenName
      , voteTokenName
      , voteNftSymbol
      , voteNftTokenName
      , fungibleSymbol
      , fungibleTokenName
      }

instance ConvertJsToPs WebApi.CancelVoteParams DaoApi.CancelVoteParams where
  convertJsToPs (WebApi.CancelVoteParams params) = do

    configSymbol <- convertJsToPs params.configSymbol
    configTokenName <- convertJsToPs params.configTokenName
    voteTokenName <- convertJsToPs params.voteTokenName
    voteNftSymbol <- convertJsToPs params.voteNftSymbol
    voteNftTokenName <- convertJsToPs params.voteNftTokenName
    fungibleSymbol <- convertJsToPs params.fungibleSymbol
    fungibleTokenName <- convertJsToPs params.fungibleTokenName

    pure $ DaoApi.CancelVoteParams
      { configSymbol
      , configTokenName
      , voteTokenName
      , voteNftSymbol
      , voteNftTokenName
      , fungibleSymbol
      , fungibleTokenName
      }

-- * VoteOnProposalParams

instance ConvertPsToJs WebApi.VoteOnProposalParams DaoApi.VoteOnProposalParams where
  convertPsToJs (DaoApi.VoteOnProposalParams params) = do

    configSymbol <- convertPsToJs params.configSymbol
    tallySymbol <- convertPsToJs params.tallySymbol
    configTokenName <- convertPsToJs params.configTokenName
    voteTokenName <- convertPsToJs params.voteTokenName
    proposalTokenName <- convertPsToJs params.proposalTokenName
    voteDirection <- convertPsToJs params.voteDirection

    pure $ WebApi.VoteOnProposalParams
      { configSymbol
      , tallySymbol
      , configTokenName
      , voteTokenName
      , proposalTokenName
      , voteDirection
      , returnAda: params.returnAda
      }

instance ConvertJsToPs WebApi.VoteOnProposalParams DaoApi.VoteOnProposalParams where
  convertJsToPs (WebApi.VoteOnProposalParams params) = do

    configSymbol <- convertJsToPs params.configSymbol
    tallySymbol <- convertJsToPs params.tallySymbol
    configTokenName <- convertJsToPs params.configTokenName
    voteTokenName <- convertJsToPs params.voteTokenName
    proposalTokenName <- convertJsToPs params.proposalTokenName
    voteDirection <- convertJsToPs params.voteDirection

    pure $ DaoApi.VoteOnProposalParams
      { configSymbol
      , tallySymbol
      , configTokenName
      , voteTokenName
      , proposalTokenName
      , voteDirection
      , returnAda: params.returnAda
      }

-- * VoteDirection

instance ConvertPsToJs WebApi.VoteDirection DaoApi.VoteDirection where
  convertPsToJs DaoApi.VoteDirection'For = pure WebApi.For
  convertPsToJs DaoApi.VoteDirection'Against = pure WebApi.Against

instance ConvertJsToPs WebApi.VoteDirection DaoApi.VoteDirection where
  convertJsToPs WebApi.For = pure DaoApi.VoteDirection'For
  convertJsToPs WebApi.Against = pure DaoApi.VoteDirection'Against

-- * TallyStateDatum

instance ConvertPsToJs WebApi.TallyStateDatum DaoApi.TallyStateDatum where
  convertPsToJs (DaoApi.TallyStateDatum params) = do

    proposal' <- convertPsToJs params.proposal

    pure $ WebApi.TallyStateDatum
      { proposal: proposal'
      , proposalEndTime: params.proposalEndTime
      , for: params.for
      , against: params.against
      }

instance ConvertJsToPs WebApi.TallyStateDatum DaoApi.TallyStateDatum where
  convertJsToPs (WebApi.TallyStateDatum params) = do

    proposal' <- convertJsToPs params.proposal

    pure $ DaoApi.TallyStateDatum
      { proposal: proposal'
      , proposalEndTime: params.proposalEndTime
      , for: params.for
      , against: params.against
      }

-- * ProposalType

instance ConvertPsToJs WebApi.ProposalType DaoApi.ProposalType where
  convertPsToJs (DaoApi.ProposalType'Upgrade symbol) = do
    hash28 <- convertPsToJs symbol
    pure $ WebApi.Upgrade hash28
  convertPsToJs (DaoApi.ProposalType'General paymentAddress amount) = do
    paymentAddress' <- convertPsToJs paymentAddress
    pure $ WebApi.General paymentAddress' amount
  convertPsToJs
    (DaoApi.ProposalType'Trip travelAgentAddress travellerAddress amount) = do
    travelAgentAddress' <- convertPsToJs travelAgentAddress
    travellerAddress' <- convertPsToJs travellerAddress
    pure $ WebApi.Trip travelAgentAddress' travellerAddress' amount

instance ConvertJsToPs WebApi.ProposalType DaoApi.ProposalType where
  convertJsToPs (WebApi.Upgrade hash28) = do
    symbol <- convertJsToPs hash28
    pure $ DaoApi.ProposalType'Upgrade symbol
  convertJsToPs (WebApi.General paymentAddress amount) = do
    paymentAddress' <- convertJsToPs paymentAddress
    pure $ DaoApi.ProposalType'General paymentAddress' amount
  convertJsToPs
    (WebApi.Trip travelAgentAddress travellerAddress amount) = do
    travelAgentAddress' <- convertJsToPs travelAgentAddress
    travellerAddress' <- convertJsToPs travellerAddress
    pure $ DaoApi.ProposalType'Trip travelAgentAddress' travellerAddress' amount

-- * Address

instance ConvertPsToJs WebApi.Address Ctl.Address where
  convertPsToJs address = do
    networkId <- ask
    pure $ WebApi.Address $ Ctl.addressWithNetworkTagToBech32 $
      Ctl.AddressWithNetworkTag
        { address: address
        , networkId
        }

instance ConvertJsToPs WebApi.Address Ctl.Address where
  convertJsToPs (WebApi.Address addr) = do
    Ctl.AddressWithNetworkTag { address } <-
      note ("Invalid address: " <> show addr) $
        Ctl.addressWithNetworkTagFromBech32 addr
    pure address

-- * TokenName

instance ConvertPsToJs WebApi.TokenName Ctl.TokenName where
  convertPsToJs = pure <<< WebApi.TokenName <<< Ctl.byteArrayToHex <<<
    Ctl.getTokenName

instance ConvertJsToPs WebApi.TokenName Ctl.TokenName where
  convertJsToPs (WebApi.TokenName tn) = do
    note ("Invalid token name: " <> show tn)
      $ Ctl.hexToByteArray tn
      >>= Ctl.mkTokenName

-- * CurrencySymbol

instance ConvertPsToJs WebApi.Hash28 Ctl.CurrencySymbol where
  convertPsToJs = pure <<< WebApi.Hash28 <<< Ctl.byteArrayToHex <<<
    Ctl.getCurrencySymbol

instance ConvertJsToPs WebApi.Hash28 Ctl.CurrencySymbol where
  convertJsToPs (WebApi.Hash28 hash28) =
    note ("Invalid currency symbol: " <> show hash28)
      $ Ctl.hexToByteArray hash28
      >>= Ctl.mkCurrencySymbol

-- * ScriptHash

instance ConvertPsToJs WebApi.ScriptHash Ctl.ScriptHash where
  convertPsToJs = pure <<< WebApi.ScriptHash <<< rawBytesToHex <<<
    scriptHashToBytes

instance ConvertJsToPs WebApi.ScriptHash Ctl.ScriptHash where
  convertJsToPs (WebApi.ScriptHash scriptHash) =
    note ("Invalid ScriptHash: " <> show scriptHash)
      $ Ctl.hexToByteArray scriptHash
      >>= scriptHashFromBytes

-- * PaymentPubKeyHash

instance ConvertPsToJs WebApi.PaymentPubKeyHash Ctl.PaymentPubKeyHash where
  convertPsToJs =
    pure
      <<< WebApi.PaymentPubKeyHash
      <<< rawBytesToHex
      <<< ed25519KeyHashToBytes
      <<< unwrap
      <<< unwrap

instance ConvertJsToPs WebApi.PaymentPubKeyHash Ctl.PaymentPubKeyHash where
  convertJsToPs (WebApi.PaymentPubKeyHash pkh) = do
    bytes <- note ("Invalid PaymentPubKeyHash: " <> show pkh) $
      Ctl.hexToByteArray pkh
    edKey <- note ("Invalid PaymentPubKeyHash: " <> show pkh) $
      ed25519KeyHashFromBytes bytes
    pure $ Ctl.PaymentPubKeyHash $ Ctl.PubKeyHash edKey

-- * TransactionHash

instance ConvertPsToJs WebApi.Hash32 Ctl.TransactionHash where
  convertPsToJs (Ctl.TransactionHash bytes) =
    pure $ WebApi.Hash32 $ Ctl.byteArrayToHex bytes

instance ConvertJsToPs WebApi.Hash32 Ctl.TransactionHash where
  convertJsToPs (WebApi.Hash32 hash32) = do
    bytes <- note ("Invalid txHash: " <> show hash32) $ Ctl.hexToByteArray
      hash32
    pure $ Ctl.TransactionHash bytes

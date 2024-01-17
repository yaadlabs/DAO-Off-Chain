{-|
Module: Dao.Web.Conversion
Description: For conversions between JavaScript and PureScript
-}
module Dao.Web.Conversion where

import Contract.Prelude

import Contract.Address
  ( Address
  , AddressWithNetworkTag(AddressWithNetworkTag)
  , PubKeyHash
  , addressWithNetworkTagFromBech32
  , addressWithNetworkTagToBech32
  ) as Ctl
import Contract.Config
  ( NetworkId
  ) as Ctl
import Contract.Prim.ByteArray (byteArrayToHex, hexToByteArray) as Ctl
import Contract.Prim.ByteArray (rawBytesToHex)
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
  ( scriptHashFromBytes
  , scriptHashToBytes
  )
import Dao.Component.Config.Params
  ( CreateConfigParams(CreateConfigParams)
  , UpgradeConfigParams(UpgradeConfigParams)
  ) as DaoApi
import Dao.Component.Proposal.Params
  ( CreateProposalParams(CreateProposalParams)
  ) as DaoApi
import Dao.Web.Types as WebApi
import Data.Either (Either(Left))
import LambdaBuffers.ApplicationTypes.Configuration
  ( DynamicConfigDatum(DynamicConfigDatum)
  ) as DaoApi
import LambdaBuffers.ApplicationTypes.Proposal
  ( ProposalType(ProposalType'General, ProposalType'Trip, ProposalType'Upgrade)
  ) as DaoApi
import LambdaBuffers.ApplicationTypes.Tally (TallyStateDatum(TallyStateDatum)) as DaoApi

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

-- * CreateConfigParams

instance
  ConvertPsToJs WebApi.CreateConfigParams DaoApi.CreateConfigParams where
  convertPsToJs (DaoApi.CreateConfigParams params) = do

    configTokenName' <- convertPsToJs params.configTokenName
    tallyNft' <- convertPsToJs params.tallyNft
    voteCurrencySymbol' <- convertPsToJs params.voteCurrencySymbol
    voteTokenName' <- convertPsToJs params.voteTokenName
    voteNft' <- convertPsToJs params.voteNft
    voteFungibleCurrencySymbol' <- convertPsToJs
      params.voteFungibleCurrencySymbol
    voteFungibleTokenName' <- convertPsToJs params.voteFungibleTokenName

    pure $ WebApi.CreateConfigParams
      { configTokenName: configTokenName'
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

instance
  ConvertJsToPs WebApi.CreateConfigParams DaoApi.CreateConfigParams where
  convertJsToPs (WebApi.CreateConfigParams params) = do

    configTokenName' <- convertJsToPs params.configTokenName
    tallyNft' <- convertJsToPs params.tallyNft
    voteCurrencySymbol' <- convertJsToPs params.voteCurrencySymbol
    voteTokenName' <- convertJsToPs params.voteTokenName
    voteNft' <- convertJsToPs params.voteNft
    voteFungibleCurrencySymbol' <- convertJsToPs
      params.voteFungibleCurrencySymbol
    voteFungibleTokenName' <- convertJsToPs params.voteFungibleTokenName

    pure $ DaoApi.CreateConfigParams
      { configTokenName: configTokenName'
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
    pure $ WebApi.ProposalType'Upgrade hash28
  convertPsToJs (DaoApi.ProposalType'General paymentAddress amount) = do
    paymentAddress' <- convertPsToJs paymentAddress
    pure $ WebApi.ProposalType'General paymentAddress' amount
  convertPsToJs
    (DaoApi.ProposalType'Trip travelAgentAddress travellerAddress amount) = do
    travelAgentAddress' <- convertPsToJs travelAgentAddress
    travellerAddress' <- convertPsToJs travellerAddress
    pure $ WebApi.ProposalType'Trip travelAgentAddress' travellerAddress' amount

instance ConvertJsToPs WebApi.ProposalType DaoApi.ProposalType where
  convertJsToPs (WebApi.ProposalType'Upgrade hash28) = do
    symbol <- convertJsToPs hash28
    pure $ DaoApi.ProposalType'Upgrade symbol
  convertJsToPs (WebApi.ProposalType'General paymentAddress amount) = do
    paymentAddress' <- convertJsToPs paymentAddress
    pure $ DaoApi.ProposalType'General paymentAddress' amount
  convertJsToPs
    (WebApi.ProposalType'Trip travelAgentAddress travellerAddress amount) = do
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

module Dao.Component.Vote.Query
  ( VoteInfo
  , mkAllVoteConstraintsAndLookups
  , referenceVoteUtxo
  , spendVoteUtxo
  , spendVoteNftUtxo
  , spendFungibleUtxo
  ) where

import Contract.Address (PaymentPubKeyHash)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData
  ( Datum(Datum)
  , OutputDatum(OutputDatum)
  , Redeemer(Redeemer)
  , fromData
  , toData
  , unitRedeemer
  )
import Contract.Prelude
  ( type (/\)
  , any
  , bind
  , discard
  , mconcat
  , negate
  , one
  , pure
  , traverse
  , unwrap
  , (#)
  , ($)
  , (&&)
  , (*)
  , (+)
  , (/)
  , (/\)
  , (<>)
  , (==)
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy, Validator)
import Contract.Transaction
  ( TransactionInput
  , TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  )
import Contract.TxConstraints as Constraints
import Contract.Value (CurrencySymbol, TokenName, Value, singleton, symbols)
import Dao.Utils.Address (addressToPaymentPubKeyHash)
import Dao.Utils.Query
  ( QueryType(Reference, Spend)
  , UtxoInfo
  , findKeyUtxoBySymbol
  , findScriptUtxoBySymbol
  )
import Dao.Utils.Query (SpendPubKeyResult, findKeyUtxoBySymbol)
import Dao.Utils.Value (countOfTokenInValue)
import Dao.Utils.Value (mkTokenName)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing))
import JS.BigInt (BigInt, fromInt)
import LambdaBuffers.ApplicationTypes.Vote
  ( VoteActionRedeemer(VoteActionRedeemer'Count)
  , VoteDatum
  , VoteDirection
  , VoteMinterActionRedeemer(VoteMinterActionRedeemer'Burn)
  )
import Type.Proxy (Proxy(Proxy))

mkAllVoteConstraintsAndLookups ::
  CurrencySymbol ->
  CurrencySymbol ->
  CurrencySymbol ->
  TokenName ->
  BigInt ->
  MintingPolicy ->
  Map TransactionInput TransactionOutputWithRefScript ->
  Contract
    ( Array
        ( (VoteDirection /\ BigInt) /\ Lookups.ScriptLookups /\
            Constraints.TxConstraints
        )
    )
mkAllVoteConstraintsAndLookups
  voteNftSymbol
  voteSymbol
  fungibleSymbol
  voteTokenName
  fungiblePercent
  votePolicyScript
  utxos =
  traverse
    ( mkVoteUtxoConstraintsAndLookups
        voteNftSymbol
        voteSymbol
        fungibleSymbol
        voteTokenName
        fungiblePercent
        votePolicyScript
    )
    (Map.toUnfoldableUnordered utxos)

mkVoteUtxoConstraintsAndLookups ::
  CurrencySymbol ->
  CurrencySymbol ->
  CurrencySymbol ->
  TokenName ->
  BigInt ->
  MintingPolicy ->
  (TransactionInput /\ TransactionOutputWithRefScript) ->
  Contract
    ( (VoteDirection /\ BigInt) /\ Lookups.ScriptLookups /\
        Constraints.TxConstraints
    )
mkVoteUtxoConstraintsAndLookups
  voteNftSymbol
  voteSymbol
  fungibleSymbol
  voteTokenName
  fungiblePercent
  votePolicyScript
  (txIn /\ txOut) =
  do
    logInfo' "Entering mkVoteUtxoConstraintsAndLookups"

    voteDatum :: VoteDatum <- liftContractM "Failed to extract datum" $
      extractOutputDatum
        txOut
    voteValue :: Value <-
      liftContractM "Vote value does not contain both vote NFT and vote token" $
        extractToken voteNftSymbol voteSymbol txOut

    voteOwnerKey :: PaymentPubKeyHash <-
      liftContractM "Cannot get pkh" $ addressToPaymentPubKeyHash $ voteDatum
        # unwrap
        # _.voteOwner

    voteNftTokenName :: TokenName <-
      liftContractM "Could not make voteNft token name" $ mkTokenName
        "vote_pass"
    fungibleTokenName :: TokenName <-
      liftContractM "Could not make voteNft token name" $ mkTokenName
        "vote_fungible"

    let
      -- If the user holds fungible tokens we need to add the calculated weight
      -- of these tokens to the vote amount
      fungibleAmount = countOfToken fungibleSymbol txOut
      fungibleVoteWeight = (fungibleAmount * fungiblePercent) / (fromInt 1000)

    let
      voteDirection' :: VoteDirection
      voteDirection' = voteDatum # unwrap # _.direction

      voteAmount :: BigInt
      voteAmount = (fromInt 1) + fungibleVoteWeight

      voteNftToken :: Value
      voteNftToken = singleton voteNftSymbol voteNftTokenName one

      fungibleToken :: Value
      fungibleToken = singleton fungibleSymbol fungibleTokenName fungibleAmount

      burnVoteValue :: Value
      burnVoteValue = singleton voteSymbol voteTokenName (negate one)

      burnVoteRedeemer :: Redeemer
      burnVoteRedeemer = Redeemer $ toData $ VoteMinterActionRedeemer'Burn

      lookups' :: Lookups.ScriptLookups
      lookups' = mconcat
        [ Lookups.unspentOutputs $
            Map.singleton txIn txOut
        , Lookups.mintingPolicy votePolicyScript
        ]

      constraints' :: Constraints.TxConstraints
      constraints' = mconcat
        [ Constraints.mustSpendScriptOutput txIn
            (Redeemer $ toData $ VoteActionRedeemer'Count)
        , Constraints.mustPayToPubKey voteOwnerKey
            (voteNftToken <> fungibleToken)
        -- ^ Return the 'voteNft', and 'fungibleToken(s)' if any
        , Constraints.mustMintValueWithRedeemer burnVoteRedeemer burnVoteValue
        ]

    pure ((voteDirection' /\ voteAmount) /\ lookups' /\ constraints')
  where
  countOfToken :: CurrencySymbol -> TransactionOutputWithRefScript -> BigInt
  countOfToken symbol txOut = countOfTokenInValue symbol value
    where
    value = txOut # unwrap # _.output # unwrap # _.amount

  extractOutputDatum :: TransactionOutputWithRefScript -> Maybe VoteDatum
  extractOutputDatum (TransactionOutputWithRefScript txOut) =
    case txOut.output # unwrap # _.datum of
      OutputDatum (Datum rawInlineDatum) -> case fromData rawInlineDatum of
        Just (datum :: VoteDatum) -> Just datum
        _ -> Nothing
      _ -> Nothing

  extractToken ::
    CurrencySymbol ->
    CurrencySymbol ->
    TransactionOutputWithRefScript ->
    Maybe Value
  extractToken voteSymbol voteNftSymbol txOut =
    let
      value = txOut # unwrap # _.output # unwrap # _.amount
    in
      if
        (any (_ == voteSymbol) $ symbols value) &&
          (any (_ == voteNftSymbol) $ symbols value) then Just value
      else Nothing

type VoteInfo = UtxoInfo VoteDatum

referenceVoteUtxo ::
  CurrencySymbol ->
  Validator ->
  Contract VoteInfo
referenceVoteUtxo voteSymbol voteValidator = do
  logInfo' "Entering referenceVoteUtxo contract"
  findScriptUtxoBySymbol
    (Proxy :: Proxy VoteDatum)
    Reference
    unitRedeemer
    voteSymbol
    voteValidator

spendVoteUtxo ::
  VoteActionRedeemer ->
  CurrencySymbol ->
  Validator ->
  Contract VoteInfo
spendVoteUtxo voteActionRedeemer voteSymbol voteValidator = do
  logInfo' "Entering spendVoteUtxo contract"
  findScriptUtxoBySymbol
    (Proxy :: Proxy VoteDatum)
    Spend
    (Redeemer $ toData $ voteActionRedeemer)
    voteSymbol
    voteValidator

spendVoteNftUtxo ::
  CurrencySymbol ->
  Map TransactionInput TransactionOutputWithRefScript ->
  Contract SpendPubKeyResult
spendVoteNftUtxo voteNftSymbol utxoMap = do
  logInfo' "Entering spendVoteNftUtxo contract"
  findKeyUtxoBySymbol voteNftSymbol utxoMap

spendFungibleUtxo ::
  CurrencySymbol ->
  Map TransactionInput TransactionOutputWithRefScript ->
  Contract SpendPubKeyResult
spendFungibleUtxo symbol utxoMap = do
  logInfo' "Entering spendFungibleUtxo contract"
  findKeyUtxoBySymbol symbol utxoMap

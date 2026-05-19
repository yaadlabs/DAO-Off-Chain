module LambdaBuffers.ApplicationTypes.Vote
  ( VoteActionRedeemer(..)
  , VoteDatum(..)
  , VoteDirection(..)
  , VoteMinterActionRedeemer(..)
  ) where

import Cardano.FromData (class FromData, fromData)
import Cardano.Plutus.Types.Address (Address)
import Cardano.ToData (class ToData, toData)
import Cardano.Types.PlutusData (PlutusData(Integer, List)) as PlutusData
import Contract.Value (TokenName)
import Data.Generic.Rep as Data.Generic.Rep
import Data.Maybe as Data.Maybe
import Data.Newtype as Data.Newtype
import Data.Show as Data.Show
import Data.Show.Generic as Data.Show.Generic
import Data.Tuple as Data.Tuple
import JS.BigInt (BigInt)
import JS.BigInt as JS.BigInt
import LambdaBuffers.Runtime.Plutus as LambdaBuffers.Runtime.Plutus
import LambdaBuffers.Runtime.Prelude as LambdaBuffers.Runtime.Prelude
import Prelude as Prelude

data VoteActionRedeemer = VoteActionRedeemer'Count | VoteActionRedeemer'Cancel

derive instance Data.Generic.Rep.Generic VoteActionRedeemer _
instance Data.Show.Show VoteActionRedeemer where
  show x = Data.Show.Generic.genericShow x

newtype VoteDatum = VoteDatum
  { proposalTokenName :: TokenName
  , direction :: VoteDirection
  , voteOwner :: Address
  , returnAda :: BigInt
  }

derive instance Data.Newtype.Newtype VoteDatum _
derive instance Data.Generic.Rep.Generic VoteDatum _
instance Data.Show.Show VoteDatum where
  show x = Data.Show.Generic.genericShow x

data VoteDirection = VoteDirection'For | VoteDirection'Against

derive instance Data.Generic.Rep.Generic VoteDirection _
instance Data.Show.Show VoteDirection where
  show x = Data.Show.Generic.genericShow x

data VoteMinterActionRedeemer
  = VoteMinterActionRedeemer'Mint
  | VoteMinterActionRedeemer'Burn

derive instance Data.Generic.Rep.Generic VoteMinterActionRedeemer _
instance Data.Show.Show VoteMinterActionRedeemer where
  show x = Data.Show.Generic.genericShow x

instance Prelude.Eq VoteDirection where
  eq =
    ( \x0 ->
        ( \x1 -> case x0 of
            VoteDirection'For -> case x1 of
              VoteDirection'For -> true

              VoteDirection'Against -> false

            VoteDirection'Against -> case x1 of
              VoteDirection'For -> false

              VoteDirection'Against -> true

        )
    )

instance ToData VoteDirection where
  toData =
    ( \x0 -> case x0 of
        VoteDirection'For -> PlutusData.Integer
          ((JS.BigInt.fromInt 0))

        VoteDirection'Against -> PlutusData.Integer
          ((JS.BigInt.fromInt 1))
    )

instance FromData VoteDirection where
  fromData =
    ( \x0 -> LambdaBuffers.Runtime.Plutus.casePlutusData
        ( ( \x1 ->
              ( \x2 -> LambdaBuffers.Runtime.Prelude.caseInt []
                  (\x3 -> Data.Maybe.Nothing)
                  x1
              )
          )
        )
        ((\x4 -> Data.Maybe.Nothing))
        ( ( \x5 -> LambdaBuffers.Runtime.Prelude.caseInt
              [ Data.Tuple.Tuple (JS.BigInt.fromInt 0)
                  (Data.Maybe.Just (VoteDirection'For))
              , Data.Tuple.Tuple (JS.BigInt.fromInt 1)
                  (Data.Maybe.Just (VoteDirection'Against))
              ]
              (\x6 -> Data.Maybe.Nothing)
              x5
          )
        )
        ((\x7 -> Data.Maybe.Nothing))
        (x0)
    )

instance Prelude.Eq VoteDatum where
  eq =
    ( \x0 ->
        ( \x1 -> Prelude.(&&)
            ( Prelude.(&&)
                ( Prelude.(&&)
                    ( Prelude.(==) ((Data.Newtype.unwrap x0).proposalTokenName)
                        ((Data.Newtype.unwrap x1).proposalTokenName)
                    )
                    ( Prelude.(==) ((Data.Newtype.unwrap x0).direction)
                        ((Data.Newtype.unwrap x1).direction)
                    )
                )
                ( Prelude.(==) ((Data.Newtype.unwrap x0).voteOwner)
                    ((Data.Newtype.unwrap x1).voteOwner)
                )
            )
            ( Prelude.(==) ((Data.Newtype.unwrap x0).returnAda)
                ((Data.Newtype.unwrap x1).returnAda)
            )
        )
    )

instance ToData VoteDatum where
  toData =
    ( \x0 -> PlutusData.List
        ( [ toData
              ((Data.Newtype.unwrap x0).proposalTokenName)
          , toData ((Data.Newtype.unwrap x0).direction)
          , toData ((Data.Newtype.unwrap x0).voteOwner)
          , toData ((Data.Newtype.unwrap x0).returnAda)
          ]
        )
    )

instance FromData VoteDatum where
  fromData =
    ( \x0 -> LambdaBuffers.Runtime.Plutus.casePlutusData
        ((\x1 -> (\x2 -> Data.Maybe.Nothing)))
        ( ( \x3 -> case x3 of
              [ x4
              , x5
              , x6
              , x7
              ] -> Prelude.(>>=) (fromData (x4))
                ( ( \x8 -> Prelude.(>>=) (fromData (x5))
                      ( ( \x9 -> Prelude.(>>=)
                            (fromData (x6))
                            ( ( \x10 -> Prelude.(>>=)
                                  (fromData (x7))
                                  ( ( \x11 -> Data.Maybe.Just
                                        ( VoteDatum
                                            { proposalTokenName: x8
                                            , direction: x9
                                            , voteOwner: x10
                                            , returnAda: x11
                                            }
                                        )
                                    )
                                  )
                              )
                            )
                        )
                      )
                  )
                )
              x12 -> Data.Maybe.Nothing
          )
        )
        ((\x13 -> Data.Maybe.Nothing))
        ((\x14 -> Data.Maybe.Nothing))
        (x0)
    )

instance Prelude.Eq VoteMinterActionRedeemer where
  eq =
    ( \x0 ->
        ( \x1 -> case x0 of
            VoteMinterActionRedeemer'Mint -> case x1 of
              VoteMinterActionRedeemer'Mint -> true

              VoteMinterActionRedeemer'Burn -> false

            VoteMinterActionRedeemer'Burn -> case x1 of
              VoteMinterActionRedeemer'Mint -> false

              VoteMinterActionRedeemer'Burn -> true

        )
    )

instance ToData VoteMinterActionRedeemer where
  toData =
    ( \x0 -> case x0 of
        VoteMinterActionRedeemer'Mint -> PlutusData.Integer
          ((JS.BigInt.fromInt 0))

        VoteMinterActionRedeemer'Burn -> PlutusData.Integer
          ((JS.BigInt.fromInt 1))
    )

instance FromData VoteMinterActionRedeemer where
  fromData =
    ( \x0 -> LambdaBuffers.Runtime.Plutus.casePlutusData
        ( ( \x1 ->
              ( \x2 -> LambdaBuffers.Runtime.Prelude.caseInt []
                  (\x3 -> Data.Maybe.Nothing)
                  x1
              )
          )
        )
        ((\x4 -> Data.Maybe.Nothing))
        ( ( \x5 -> LambdaBuffers.Runtime.Prelude.caseInt
              [ Data.Tuple.Tuple (JS.BigInt.fromInt 0)
                  (Data.Maybe.Just (VoteMinterActionRedeemer'Mint))
              , Data.Tuple.Tuple (JS.BigInt.fromInt 1)
                  (Data.Maybe.Just (VoteMinterActionRedeemer'Burn))
              ]
              (\x6 -> Data.Maybe.Nothing)
              x5
          )
        )
        ((\x7 -> Data.Maybe.Nothing))
        (x0)
    )

instance Prelude.Eq VoteActionRedeemer where
  eq =
    ( \x0 ->
        ( \x1 -> case x0 of
            VoteActionRedeemer'Count -> case x1 of
              VoteActionRedeemer'Count -> true

              VoteActionRedeemer'Cancel -> false

            VoteActionRedeemer'Cancel -> case x1 of
              VoteActionRedeemer'Count -> false

              VoteActionRedeemer'Cancel -> true

        )
    )

instance ToData VoteActionRedeemer where
  toData =
    ( \x0 -> case x0 of
        VoteActionRedeemer'Count -> PlutusData.Integer
          ((JS.BigInt.fromInt 0))

        VoteActionRedeemer'Cancel -> PlutusData.Integer
          ((JS.BigInt.fromInt 1))
    )

instance FromData VoteActionRedeemer where
  fromData =
    ( \x0 -> LambdaBuffers.Runtime.Plutus.casePlutusData
        ( ( \x1 ->
              ( \x2 -> LambdaBuffers.Runtime.Prelude.caseInt []
                  (\x3 -> Data.Maybe.Nothing)
                  x1
              )
          )
        )
        ((\x4 -> Data.Maybe.Nothing))
        ( ( \x5 -> LambdaBuffers.Runtime.Prelude.caseInt
              [ Data.Tuple.Tuple (JS.BigInt.fromInt 0)
                  (Data.Maybe.Just (VoteActionRedeemer'Count))
              , Data.Tuple.Tuple (JS.BigInt.fromInt 1)
                  (Data.Maybe.Just (VoteActionRedeemer'Cancel))
              ]
              (\x6 -> Data.Maybe.Nothing)
              x5
          )
        )
        ((\x7 -> Data.Maybe.Nothing))
        (x0)
    )

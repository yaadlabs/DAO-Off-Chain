module LambdaBuffers.ApplicationTypes.Vote
  ( VoteActionRedeemer(..)
  , VoteDatum(..)
  , VoteDirection(..)
  , VoteMinterActionRedeemer(..)
  ) where

import Ctl.Internal.FromData as Ctl.Internal.FromData
import Ctl.Internal.Plutus.Types.Address (Address)
import Ctl.Internal.ToData as Ctl.Internal.ToData
import Ctl.Internal.Types.PlutusData as Ctl.Internal.Types.PlutusData
import Ctl.Internal.Types.TokenName (TokenName)
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

instance Ctl.Internal.ToData.ToData VoteDirection where
  toData =
    ( \x0 -> case x0 of
        VoteDirection'For -> Ctl.Internal.Types.PlutusData.Integer
          ((JS.BigInt.fromInt 0))

        VoteDirection'Against -> Ctl.Internal.Types.PlutusData.Integer
          ((JS.BigInt.fromInt 1))
    )

instance Ctl.Internal.FromData.FromData VoteDirection where
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

instance Ctl.Internal.ToData.ToData VoteDatum where
  toData =
    ( \x0 -> Ctl.Internal.Types.PlutusData.List
        ( [ Ctl.Internal.ToData.toData
              ((Data.Newtype.unwrap x0).proposalTokenName)
          , Ctl.Internal.ToData.toData ((Data.Newtype.unwrap x0).direction)
          , Ctl.Internal.ToData.toData ((Data.Newtype.unwrap x0).voteOwner)
          , Ctl.Internal.ToData.toData ((Data.Newtype.unwrap x0).returnAda)
          ]
        )
    )

instance Ctl.Internal.FromData.FromData VoteDatum where
  fromData =
    ( \x0 -> LambdaBuffers.Runtime.Plutus.casePlutusData
        ((\x1 -> (\x2 -> Data.Maybe.Nothing)))
        ( ( \x3 -> case x3 of
              [ x4
              , x5
              , x6
              , x7
              ] -> Prelude.(>>=) (Ctl.Internal.FromData.fromData (x4))
                ( ( \x8 -> Prelude.(>>=) (Ctl.Internal.FromData.fromData (x5))
                      ( ( \x9 -> Prelude.(>>=)
                            (Ctl.Internal.FromData.fromData (x6))
                            ( ( \x10 -> Prelude.(>>=)
                                  (Ctl.Internal.FromData.fromData (x7))
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

instance Ctl.Internal.ToData.ToData VoteMinterActionRedeemer where
  toData =
    ( \x0 -> case x0 of
        VoteMinterActionRedeemer'Mint -> Ctl.Internal.Types.PlutusData.Integer
          ((JS.BigInt.fromInt 0))

        VoteMinterActionRedeemer'Burn -> Ctl.Internal.Types.PlutusData.Integer
          ((JS.BigInt.fromInt 1))
    )

instance Ctl.Internal.FromData.FromData VoteMinterActionRedeemer where
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

instance Ctl.Internal.ToData.ToData VoteActionRedeemer where
  toData =
    ( \x0 -> case x0 of
        VoteActionRedeemer'Count -> Ctl.Internal.Types.PlutusData.Integer
          ((JS.BigInt.fromInt 0))

        VoteActionRedeemer'Cancel -> Ctl.Internal.Types.PlutusData.Integer
          ((JS.BigInt.fromInt 1))
    )

instance Ctl.Internal.FromData.FromData VoteActionRedeemer where
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

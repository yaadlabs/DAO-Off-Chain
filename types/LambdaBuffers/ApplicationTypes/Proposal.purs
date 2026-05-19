module LambdaBuffers.ApplicationTypes.Proposal (ProposalType(..)) where

import Cardano.FromData (class FromData, fromData)
import Cardano.Plutus.Types.Address (Address)
import Cardano.ToData (class ToData, toData)
import Cardano.Types.PlutusData (PlutusData(Integer, List)) as PlutusData
import Contract.Value (CurrencySymbol)
import Contract.Value (TokenName)
import Data.Generic.Rep as Data.Generic.Rep
import Data.Maybe as Data.Maybe
import Data.Show as Data.Show
import Data.Show.Generic as Data.Show.Generic
import Data.Tuple as Data.Tuple
import JS.BigInt (BigInt)
import JS.BigInt as JS.BigInt
import LambdaBuffers.Runtime.Plutus as LambdaBuffers.Runtime.Plutus
import LambdaBuffers.Runtime.Prelude as LambdaBuffers.Runtime.Prelude
import Prelude as Prelude

data ProposalType
  = ProposalType'Upgrade CurrencySymbol
  | ProposalType'General Address BigInt
  | ProposalType'Trip Address Address BigInt

derive instance Data.Generic.Rep.Generic ProposalType _
instance Data.Show.Show ProposalType where
  show x = Data.Show.Generic.genericShow x

instance Prelude.Eq ProposalType where
  eq =
    ( \x0 ->
        ( \x1 -> case x0 of
            ProposalType'Upgrade x2 -> case x1 of
              ProposalType'Upgrade x3 -> Prelude.(==) (x2) (x3)

              ProposalType'General x4 x5 -> false

              ProposalType'Trip x6 x7 x8 -> false

            ProposalType'General x9 x10 -> case x1 of
              ProposalType'Upgrade x11 -> false

              ProposalType'General x12 x13 -> Prelude.(&&)
                (Prelude.(==) (x9) (x12))
                (Prelude.(==) (x10) (x13))

              ProposalType'Trip x14 x15 x16 -> false

            ProposalType'Trip x17 x18 x19 -> case x1 of
              ProposalType'Upgrade x20 -> false

              ProposalType'General x21 x22 -> false

              ProposalType'Trip x23 x24 x25 -> Prelude.(&&)
                ( Prelude.(&&) (Prelude.(==) (x17) (x23))
                    (Prelude.(==) (x18) (x24))
                )
                (Prelude.(==) (x19) (x25))

        )
    )

instance ToData ProposalType where
  toData =
    ( \x0 -> case x0 of
        ProposalType'Upgrade x1 -> LambdaBuffers.Runtime.Plutus.pdConstr
          ((JS.BigInt.fromInt 0))
          ([ toData (x1) ])

        ProposalType'General x2 x3 -> LambdaBuffers.Runtime.Plutus.pdConstr
          ((JS.BigInt.fromInt 1))
          ( [ toData (x2)
            , toData (x3)
            ]
          )

        ProposalType'Trip x4 x5 x6 -> LambdaBuffers.Runtime.Plutus.pdConstr
          ((JS.BigInt.fromInt 2))
          ( [ toData (x4)
            , toData (x5)
            , toData (x6)
            ]
          )
    )

instance FromData ProposalType where
  fromData =
    ( \x0 -> LambdaBuffers.Runtime.Plutus.casePlutusData
        ( ( \x1 ->
              ( \x2 -> LambdaBuffers.Runtime.Prelude.caseInt
                  [ Data.Tuple.Tuple (JS.BigInt.fromInt 0)
                      ( case x2 of
                          [ x3 ] -> Prelude.(>>=)
                            (fromData (x3))
                            ((\x4 -> Data.Maybe.Just (ProposalType'Upgrade x4)))
                          x5 -> Data.Maybe.Nothing
                      )
                  , Data.Tuple.Tuple (JS.BigInt.fromInt 1)
                      ( case x2 of
                          [ x6
                          , x7
                          ] -> Prelude.(>>=)
                            (fromData (x6))
                            ( ( \x8 -> Prelude.(>>=)
                                  (fromData (x7))
                                  ( ( \x9 -> Data.Maybe.Just
                                        (ProposalType'General x8 x9)
                                    )
                                  )
                              )
                            )
                          x10 -> Data.Maybe.Nothing
                      )
                  , Data.Tuple.Tuple (JS.BigInt.fromInt 2)
                      ( case x2 of
                          [ x11
                          , x12
                          , x13
                          ] -> Prelude.(>>=)
                            (fromData (x11))
                            ( ( \x14 -> Prelude.(>>=)
                                  (fromData (x12))
                                  ( ( \x15 -> Prelude.(>>=)
                                        (fromData (x13))
                                        ( ( \x16 -> Data.Maybe.Just
                                              (ProposalType'Trip x14 x15 x16)
                                          )
                                        )
                                    )
                                  )
                              )
                            )
                          x17 -> Data.Maybe.Nothing
                      )
                  ]
                  (\x18 -> Data.Maybe.Nothing)
                  x1
              )
          )
        )
        ((\x19 -> Data.Maybe.Nothing))
        ( ( \x20 -> LambdaBuffers.Runtime.Prelude.caseInt []
              (\x21 -> Data.Maybe.Nothing)
              x20
          )
        )
        ((\x22 -> Data.Maybe.Nothing))
        (x0)
    )

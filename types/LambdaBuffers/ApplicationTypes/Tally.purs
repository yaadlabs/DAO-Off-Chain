module LambdaBuffers.ApplicationTypes.Tally (TallyStateDatum(..)) where

import Contract.Time (POSIXTime)
import Ctl.Internal.FromData as Ctl.Internal.FromData
import Ctl.Internal.ToData as Ctl.Internal.ToData
import Ctl.Internal.Types.PlutusData as Ctl.Internal.Types.PlutusData
import Data.Generic.Rep as Data.Generic.Rep
import Data.Maybe as Data.Maybe
import Data.Newtype as Data.Newtype
import Data.Show as Data.Show
import Data.Show.Generic as Data.Show.Generic
import JS.BigInt (BigInt)
import LambdaBuffers.ApplicationTypes.Proposal as LambdaBuffers.ApplicationTypes.Proposal
import LambdaBuffers.Runtime.Plutus as LambdaBuffers.Runtime.Plutus
import Prelude as Prelude

newtype TallyStateDatum = TallyStateDatum
  { proposal :: LambdaBuffers.ApplicationTypes.Proposal.ProposalType
  , proposalEndTime :: POSIXTime
  , for :: BigInt
  , against :: BigInt
  }

derive instance Data.Newtype.Newtype TallyStateDatum _
derive instance Data.Generic.Rep.Generic TallyStateDatum _
instance Data.Show.Show TallyStateDatum where
  show x = Data.Show.Generic.genericShow x

instance Prelude.Eq TallyStateDatum where
  eq =
    ( \x0 ->
        ( \x1 -> Prelude.(&&)
            ( Prelude.(&&)
                ( Prelude.(&&)
                    ( Prelude.(==) ((Data.Newtype.unwrap x0).proposal)
                        ((Data.Newtype.unwrap x1).proposal)
                    )
                    ( Prelude.(==) ((Data.Newtype.unwrap x0).proposalEndTime)
                        ((Data.Newtype.unwrap x1).proposalEndTime)
                    )
                )
                ( Prelude.(==) ((Data.Newtype.unwrap x0).for)
                    ((Data.Newtype.unwrap x1).for)
                )
            )
            ( Prelude.(==) ((Data.Newtype.unwrap x0).against)
                ((Data.Newtype.unwrap x1).against)
            )
        )
    )

instance Ctl.Internal.ToData.ToData TallyStateDatum where
  toData =
    ( \x0 -> Ctl.Internal.Types.PlutusData.List
        ( [ Ctl.Internal.ToData.toData ((Data.Newtype.unwrap x0).proposal)
          , Ctl.Internal.ToData.toData
              ((Data.Newtype.unwrap x0).proposalEndTime)
          , Ctl.Internal.ToData.toData ((Data.Newtype.unwrap x0).for)
          , Ctl.Internal.ToData.toData ((Data.Newtype.unwrap x0).against)
          ]
        )
    )

instance Ctl.Internal.FromData.FromData TallyStateDatum where
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
                                        ( TallyStateDatum
                                            { proposal: x8
                                            , proposalEndTime: x9
                                            , for: x10
                                            , against: x11
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

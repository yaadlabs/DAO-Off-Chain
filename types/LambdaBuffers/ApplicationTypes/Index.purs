module LambdaBuffers.ApplicationTypes.Index (IndexNftDatum(..)) where

import Ctl.Internal.FromData as Ctl.Internal.FromData
import Ctl.Internal.ToData as Ctl.Internal.ToData
import Data.Generic.Rep as Data.Generic.Rep
import Data.Maybe as Data.Maybe
import Data.Newtype as Data.Newtype
import Data.Show as Data.Show
import Data.Show.Generic as Data.Show.Generic
import Prelude as Prelude
import JS.BigInt (BigInt)

newtype IndexNftDatum = IndexNftDatum { index :: BigInt }

derive instance Data.Newtype.Newtype IndexNftDatum _
derive instance Data.Generic.Rep.Generic IndexNftDatum _
instance Data.Show.Show IndexNftDatum where
  show x = Data.Show.Generic.genericShow x

instance Prelude.Eq IndexNftDatum where
  eq = (\x0 -> (\x1 -> Prelude.(==) ((Data.Newtype.unwrap x0).index) ((Data.Newtype.unwrap x1).index)))

instance Ctl.Internal.ToData.ToData IndexNftDatum where
  toData = (\x0 -> Ctl.Internal.ToData.toData ((Data.Newtype.unwrap x0).index))

instance Ctl.Internal.FromData.FromData IndexNftDatum where
  fromData = (\x0 -> Prelude.(>>=) (Ctl.Internal.FromData.fromData (x0)) ((\x1 -> Data.Maybe.Just (IndexNftDatum { index: x1 }))))

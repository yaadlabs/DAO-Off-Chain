module LambdaBuffers.ApplicationTypes.Index (IndexDatum(..)) where

import Cardano.FromData (class FromData, fromData)
import Cardano.ToData (class ToData, toData)
import Data.Generic.Rep as Data.Generic.Rep
import Data.Maybe as Data.Maybe
import Data.Newtype as Data.Newtype
import Data.Show as Data.Show
import Data.Show.Generic as Data.Show.Generic
import JS.BigInt (BigInt)
import Prelude as Prelude

newtype IndexDatum = IndexDatum { index :: BigInt }

derive instance Data.Newtype.Newtype IndexDatum _
derive instance Data.Generic.Rep.Generic IndexDatum _
instance Data.Show.Show IndexDatum where
  show x = Data.Show.Generic.genericShow x

instance Prelude.Eq IndexDatum where
  eq =
    ( \x0 ->
        ( \x1 -> Prelude.(==) ((Data.Newtype.unwrap x0).index)
            ((Data.Newtype.unwrap x1).index)
        )
    )

instance ToData IndexDatum where
  toData = (\x0 -> toData ((Data.Newtype.unwrap x0).index))

instance FromData IndexDatum where
  fromData =
    ( \x0 -> Prelude.(>>=) (fromData (x0))
        ((\x1 -> Data.Maybe.Just (IndexDatum { index: x1 })))
    )

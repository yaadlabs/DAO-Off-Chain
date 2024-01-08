module LambdaBuffers.ApplicationConfig.Scripts
  ( Script(..)
  , Scripts(..)
  , Bytes(..)
  ) where

import Prelude

import Data.ArrayBuffer.Typed as ArrayBuffer
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Either as Data.Either
import Data.Generic.Rep as Data.Generic.Rep
import Data.Newtype as Data.Newtype
import Data.Show as Data.Show
import Data.Show.Generic as Data.Show.Generic
import Data.Tuple as Data.Tuple
import Effect.Unsafe (unsafePerformEffect)
import LambdaBuffers.Runtime.Prelude as LambdaBuffers.Runtime.Prelude
import Prelude as Prelude

newtype Script = Script Bytes

derive instance Data.Newtype.Newtype Script _
derive instance Data.Generic.Rep.Generic Script _
instance Data.Show.Show Script where
  show x = Data.Show.Generic.genericShow x

newtype Scripts = Scripts { configPolicy :: Script }

derive instance Data.Newtype.Newtype Scripts _
derive instance Data.Generic.Rep.Generic Scripts _
instance Data.Show.Show Scripts where
  show x = Data.Show.Generic.genericShow x

instance Prelude.Eq Script where
  eq =
    ( \x0 ->
        ( \x1 ->
            let Script x2 = x0 in let Script x3 = x1 in Prelude.(==) (x2) (x3)
        )
    )

instance LambdaBuffers.Runtime.Prelude.Json Script where
  toJson =
    (\x0 -> let Script x1 = x0 in LambdaBuffers.Runtime.Prelude.toJson (x1))
  fromJson =
    ( \x0 -> Prelude.(>>=) (LambdaBuffers.Runtime.Prelude.fromJson (x0))
        ((\x1 -> Data.Either.Right (Script x1)))
    )

instance Prelude.Eq Scripts where
  eq =
    ( \x0 ->
        ( \x1 -> Prelude.(==) ((Data.Newtype.unwrap x0).configPolicy)
            ((Data.Newtype.unwrap x1).configPolicy)
        )
    )

instance LambdaBuffers.Runtime.Prelude.Json Scripts where
  toJson =
    ( \x0 -> LambdaBuffers.Runtime.Prelude.jsonObject
        ( [ ( Data.Tuple.Tuple ("configPolicy")
                ( LambdaBuffers.Runtime.Prelude.toJson
                    ((Data.Newtype.unwrap x0).configPolicy)
                )
            )
          ]
        )
    )
  fromJson = LambdaBuffers.Runtime.Prelude.caseJsonObject
    ( ( \x0 -> LambdaBuffers.Runtime.Prelude.jsonField ("configPolicy") (x0)
          ( ( \x1 -> Prelude.(>>=) (LambdaBuffers.Runtime.Prelude.fromJson (x1))
                ((\x2 -> Data.Either.Right (Scripts { configPolicy: x2 })))
            )
          )
      )
    )

newtype Bytes = Bytes Uint8Array

derive instance ntBytes :: Data.Newtype.Newtype Bytes _

instance showBytes :: Data.Show.Show Bytes where
  show = Data.Newtype.unwrap >>> LambdaBuffers.Runtime.Prelude.toJson >>> show

instance eqBytes :: Prelude.Eq Bytes where
  eq l r = unsafePerformEffect $ ArrayBuffer.eq (Data.Newtype.unwrap l)
    (Data.Newtype.unwrap r)

derive newtype instance jsonBytes :: LambdaBuffers.Runtime.Prelude.Json Bytes

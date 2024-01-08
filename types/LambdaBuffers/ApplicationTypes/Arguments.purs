module LambdaBuffers.ApplicationTypes.Arguments
  ( ConfigurationValidatorConfig(..)
  , NftConfig(..)
  ) where

import Contract.Transaction (TransactionInput)
import Contract.Value (CurrencySymbol, TokenName)
import Ctl.Internal.FromData as Ctl.Internal.FromData
import Ctl.Internal.ToData as Ctl.Internal.ToData
import Ctl.Internal.Types.PlutusData as Ctl.Internal.Types.PlutusData
import Data.Generic.Rep as Data.Generic.Rep
import Data.Maybe as Data.Maybe
import Data.Newtype as Data.Newtype
import Data.Show as Data.Show
import Data.Show.Generic as Data.Show.Generic
import LambdaBuffers.Runtime.Plutus as LambdaBuffers.Runtime.Plutus
import Ctl.Internal.Types.BigNum as BigNum
import Prelude as Prelude

newtype ConfigurationValidatorConfig = ConfigurationValidatorConfig
  { cvcConfigNftCurrencySymbol :: CurrencySymbol
  , cvcConfigNftTokenName :: TokenName
  }

derive instance Data.Newtype.Newtype ConfigurationValidatorConfig _
derive instance Data.Generic.Rep.Generic ConfigurationValidatorConfig _
instance Data.Show.Show ConfigurationValidatorConfig where
  show x = Data.Show.Generic.genericShow x

newtype NftConfig = NftConfig
  { ncInitialUtxo :: TransactionInput
  , ncTokenName :: TokenName
  }

derive instance Data.Newtype.Newtype NftConfig _
derive instance Data.Generic.Rep.Generic NftConfig _
instance Data.Show.Show NftConfig where
  show x = Data.Show.Generic.genericShow x

instance Prelude.Eq NftConfig where
  eq =
    ( \x0 ->
        ( \x1 -> Prelude.(&&)
            ( Prelude.(==) ((Data.Newtype.unwrap x0).ncInitialUtxo)
                ((Data.Newtype.unwrap x1).ncInitialUtxo)
            )
            ( Prelude.(==) ((Data.Newtype.unwrap x0).ncTokenName)
                ((Data.Newtype.unwrap x1).ncTokenName)
            )
        )
    )

instance Ctl.Internal.ToData.ToData NftConfig where
  toData =
    ( \x0 -> Ctl.Internal.Types.PlutusData.Constr (BigNum.fromInt 0)
        ( [ Ctl.Internal.ToData.toData ((Data.Newtype.unwrap x0).ncInitialUtxo)
          , Ctl.Internal.ToData.toData ((Data.Newtype.unwrap x0).ncTokenName)
          ]
        )
    )

instance Ctl.Internal.FromData.FromData NftConfig where
  fromData =
    ( \x0 -> LambdaBuffers.Runtime.Plutus.casePlutusData
        ((\x1 -> (\x2 -> Data.Maybe.Nothing)))
        ( ( \x3 -> case x3 of
              [ x4
              , x5
              ] -> Prelude.(>>=) (Ctl.Internal.FromData.fromData (x4))
                ( ( \x6 -> Prelude.(>>=) (Ctl.Internal.FromData.fromData (x5))
                      ( ( \x7 -> Data.Maybe.Just
                            ( NftConfig
                                { ncInitialUtxo: x6
                                , ncTokenName: x7
                                }
                            )
                        )
                      )
                  )
                )
              x8 -> Data.Maybe.Nothing
          )
        )
        ((\x9 -> Data.Maybe.Nothing))
        ((\x10 -> Data.Maybe.Nothing))
        (x0)
    )

instance Prelude.Eq ConfigurationValidatorConfig where
  eq =
    ( \x0 ->
        ( \x1 -> Prelude.(&&)
            ( Prelude.(==) ((Data.Newtype.unwrap x0).cvcConfigNftCurrencySymbol)
                ((Data.Newtype.unwrap x1).cvcConfigNftCurrencySymbol)
            )
            ( Prelude.(==) ((Data.Newtype.unwrap x0).cvcConfigNftTokenName)
                ((Data.Newtype.unwrap x1).cvcConfigNftTokenName)
            )
        )
    )

instance Ctl.Internal.ToData.ToData ConfigurationValidatorConfig where
  toData =
    ( \x0 -> Ctl.Internal.Types.PlutusData.List
        ( [ Ctl.Internal.ToData.toData
              ((Data.Newtype.unwrap x0).cvcConfigNftCurrencySymbol)
          , Ctl.Internal.ToData.toData
              ((Data.Newtype.unwrap x0).cvcConfigNftTokenName)
          ]
        )
    )

instance Ctl.Internal.FromData.FromData ConfigurationValidatorConfig where
  fromData =
    ( \x0 -> LambdaBuffers.Runtime.Plutus.casePlutusData
        ((\x1 -> (\x2 -> Data.Maybe.Nothing)))
        ( ( \x3 -> case x3 of
              [ x4
              , x5
              ] -> Prelude.(>>=) (Ctl.Internal.FromData.fromData (x4))
                ( ( \x6 -> Prelude.(>>=) (Ctl.Internal.FromData.fromData (x5))
                      ( ( \x7 -> Data.Maybe.Just
                            ( ConfigurationValidatorConfig
                                { cvcConfigNftCurrencySymbol: x6
                                , cvcConfigNftTokenName: x7
                                }
                            )
                        )
                      )
                  )
                )
              x8 -> Data.Maybe.Nothing
          )
        )
        ((\x9 -> Data.Maybe.Nothing))
        ((\x10 -> Data.Maybe.Nothing))
        (x0)
    )

module ScriptArguments.Types
  ( ConfigPolicyParams(..)
  , ValidatorParams(..)
  , IndexPolicyParams(..)
  , TallyPolicyParams(..)
  ) where

import Prelude

import Contract.PlutusData
  ( class FromData
  , class ToData
  , genericFromData
  , genericToData
  )
import Contract.Transaction (TransactionInput)
import Contract.Value (CurrencySymbol, TokenName)
import Ctl.Internal.Plutus.Types.DataSchema
  ( class HasPlutusSchema
  , type (:+)
  , type (:=)
  , type (@@)
  , I
  , PNil
  )
import Ctl.Internal.Serialization.Hash (ScriptHash)
import Ctl.Internal.TypeLevel.Nat (Z)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)

newtype ConfigPolicyParams = ConfigPolicyParams
  { cpInitialUtxo :: TransactionInput
  , cpTokenName :: TokenName
  }

instance
  HasPlutusSchema ConfigPolicyParams
    ( "ConfigPolicyParams"
        :=
          ( "cpInitialUtxo" := I TransactionInput
              :+ "cpTokenName"
              := I TokenName
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

instance ToData ConfigPolicyParams where
  toData = genericToData

instance FromData ConfigPolicyParams where
  fromData = genericFromData

derive instance Generic ConfigPolicyParams _

derive instance Newtype ConfigPolicyParams _

derive newtype instance Eq ConfigPolicyParams

derive newtype instance Show ConfigPolicyParams

newtype ValidatorParams = ValidatorParams
  { vpConfigSymbol :: CurrencySymbol
  , vpConfigTokenName :: TokenName
  }

instance
  HasPlutusSchema ValidatorParams
    ( "ValidatorParams"
        :=
          ( "vpConfigSymbol" := I CurrencySymbol
              :+ "vpConfigTokenName"
              := I TokenName
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

instance ToData ValidatorParams where
  toData = genericToData

instance FromData ValidatorParams where
  fromData = genericFromData

derive instance Generic ValidatorParams _

derive instance Newtype ValidatorParams _

derive newtype instance Eq ValidatorParams

derive newtype instance Show ValidatorParams

newtype IndexPolicyParams = IndexPolicyParams
  { ipInitialUtxo :: TransactionInput
  , ipTokenName :: TokenName
  , ipIndexValidator :: ScriptHash
  }

instance
  HasPlutusSchema IndexPolicyParams
    ( "IndexPolicyParams"
        :=
          ( "ipInitialUtxo" := I TransactionInput
              :+ "ipTokenName"
              := I TokenName
              :+ "ipIndexValidator"
              := I ScriptHash
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

instance ToData IndexPolicyParams where
  toData = genericToData

instance FromData IndexPolicyParams where
  fromData = genericFromData

derive instance Generic IndexPolicyParams _

derive instance Newtype IndexPolicyParams _

derive newtype instance Eq IndexPolicyParams

derive newtype instance Show IndexPolicyParams

newtype TallyPolicyParams = TallyPolicyParams
  { tpIndexSymbol :: CurrencySymbol
  , tpIndexTokenName :: TokenName
  , tpConfigSymbol :: CurrencySymbol
  , tpConfigTokenName :: TokenName
  }

instance
  HasPlutusSchema TallyPolicyParams
    ( "TallyPolicyParams"
        :=
          ( "tpIndexSymbol" := I CurrencySymbol
              :+ "tpIndexTokenName"
              := I TokenName
              :+ "tpConfigSymbol"
              := I CurrencySymbol
              :+ "tpConfigTokenName"
              := I TokenName
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

instance ToData TallyPolicyParams where
  toData = genericToData

instance FromData TallyPolicyParams where
  fromData = genericFromData

derive instance Generic TallyPolicyParams _

derive instance Newtype TallyPolicyParams _

derive newtype instance Eq TallyPolicyParams

derive newtype instance Show TallyPolicyParams

module ScriptArguments.Types 
  ( NftConfig(..)
  , ConfigurationValidatorConfig(..) 
  , IndexNftConfig(..)
  , TallyNftConfig(..)
  ) where

import Prelude
import Contract.Value (CurrencySymbol, TokenName)
import Contract.Transaction (TransactionInput)
import Ctl.Internal.Serialization.Hash (ScriptHash)
import Ctl.Internal.Plutus.Types.DataSchema (class HasPlutusSchema, type (:+), type (:=), type (@@), I, PNil)
import Ctl.Internal.TypeLevel.Nat (Z)
import Contract.PlutusData (class FromData, class ToData, genericFromData, genericToData)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)

newtype NftConfig = NftConfig
  { ncInitialUtxo :: TransactionInput
  , ncTokenName :: TokenName
  }

instance
  HasPlutusSchema NftConfig
    ( "NftConfig"
        :=
          ( "ncInitialUtxo" := I TransactionInput
              :+ "ncTokenName"
              := I TokenName
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

instance ToData NftConfig where
  toData = genericToData

instance FromData NftConfig where
  fromData = genericFromData

derive instance Generic NftConfig _

derive instance Newtype NftConfig _

derive newtype instance Eq NftConfig

derive newtype instance Show NftConfig

newtype ConfigurationValidatorConfig = ConfigurationValidatorConfig
  { cvcConfigNftCurrencySymbol :: CurrencySymbol
  , cvcConfigNftTokenName :: TokenName
  }

instance
  HasPlutusSchema ConfigurationValidatorConfig
    ( "ConfigurationValidatorConfig"
        :=
          ( "cvcConfigNftCurrencySymbol" := I CurrencySymbol
              :+ "cvcConfigNftTokenName"
              := I TokenName
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

instance ToData ConfigurationValidatorConfig where
  toData = genericToData

instance FromData ConfigurationValidatorConfig where
  fromData = genericFromData

derive instance Generic ConfigurationValidatorConfig _

derive instance Newtype ConfigurationValidatorConfig _

derive newtype instance Eq ConfigurationValidatorConfig

derive newtype instance Show ConfigurationValidatorConfig

newtype IndexNftConfig = IndexNftConfig
  { incInitialUtxo :: TransactionInput
  , incTokenName :: TokenName
  , incIndexValidator :: ScriptHash
  }

instance
  HasPlutusSchema IndexNftConfig
    ( "IndexNftConfig"
        :=
          ( "incInitialUtxo" := I TransactionInput
              :+ "incTokenName"
              := I TokenName
              :+ "incIndexValidator"
              := I ScriptHash
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

instance ToData IndexNftConfig where
  toData = genericToData

instance FromData IndexNftConfig where
  fromData = genericFromData

derive instance Generic IndexNftConfig _

derive instance Newtype IndexNftConfig _

derive newtype instance Eq IndexNftConfig

derive newtype instance Show IndexNftConfig

newtype TallyNftConfig = TallyNftConfig
  { tncConfigNftCurrencySymbol :: CurrencySymbol
  , tncConfigNftTokenName :: TokenName
  , tncIndexNftPolicyId :: CurrencySymbol
  , tncIndexNftTokenName :: TokenName
  }
    
instance
  HasPlutusSchema TallyNftConfig
    ( "TallyNftConfig"
        :=
          ( "tncConfigNftCurrencySymbol" := I CurrencySymbol
              :+ "tncConfigNftTokenName"
              := I TokenName
              :+ "tncIndexNftPolicyId"
              := I CurrencySymbol
              :+ "tncIndexNftTokenName"
              := I TokenName
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

instance ToData TallyNftConfig where
  toData = genericToData

instance FromData TallyNftConfig where
  fromData = genericFromData

derive instance Generic TallyNftConfig _

derive instance Newtype TallyNftConfig _

derive newtype instance Eq TallyNftConfig

derive newtype instance Show TallyNftConfig

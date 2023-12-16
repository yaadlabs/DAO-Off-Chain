module Scripts.ConfigValidator
  ( configValidatorScript
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.Scripts (Validator(Validator))
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Control.Monad.Error.Class (liftMaybe)
import Effect.Exception (error)
import LambdaBuffers.Runtime.Prelude 
import LambdaBuffers.ApplicationConfig.Scripts (Scripts(..), Bytes(..))
import Ctl.Internal.Types.Scripts (plutusV2Script)
import Contract.Scripts (Validator(..), validatorHash)
import Data.Newtype (unwrap)
import Contract.Prim.ByteArray (ByteArray(..), byteArrayFromAscii)
import Node.FS.Sync as NodeFS
import Effect.Exception (throw)
import Node.Encoding (Encoding(..))
import Aeson as Aeson
import Effect.Unsafe (unsafePerformEffect)
import Data.TextEncoder (encodeUtf8)

configValidatorScript :: Validator
configValidatorScript = Validator $ plutusV2Script configValidatorByteArray

configValidatorByteArray :: ByteArray
configValidatorByteArray = toJsonBytes' configValidatorJson

toJsonBytes' :: forall a. Json a => a -> ByteArray
toJsonBytes' = ByteArray <<< encodeUtf8 <<< toJsonString

configValidatorJson :: Json String => String
configValidatorJson = 
  either 
    (\_ -> error' "Error config validator: fromJsonString") 
    identity 
    (fromJsonString configValidatorString)

error' :: forall a. String -> a
error' = unsafePerformEffect <<< throw

configValidatorString :: String
configValidatorString = "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sABFjdUACRkpmauaMCcwLAARMjIyMjIyMjIyEjMzABAJAHADACMwC3XGroTV0QAimZq5owMAARMhIiMAIAQ1dCYGQAQqZmrmjAvABEyEiIwAQBDdcauhMDIAIVMzVzRgXAAgNixgZAAm6o1dCACauiACMzAIdcAOauhABNXRGBaAGpmauaMCgwLQARMwFzALNXQmBYACZgCAFGroTV0RgWAAixuqNXQmBWACLG6oAEiMjJTM1c0YFAAImA6YAhq6EwLAAhUzNXNGBSACAiLGBYACbqgASMzACdc60AEiIzArIjMzVXPgAkBARkZgRmYDBgDmBcACYAxgWgAmAIauiADNXQgBARm6sAEiMwKSIzM1Vz4AJAPEZgQGAKauhACMAM1dEAEBCbrAASMjJTM1c0YEwAImQkREYAgApgCGroTAnACFTM1c0YEoAImQkREYAQApgCmroTAnACFTM1c0YEgAImQkREYAIApgDmroTAnACFTM1c0YEYAImQkREYAYApuuNXQmBOAELGBOACbqgASMjJTM1c0ZuHSAMABESIiAUFTM1c0ZuHSAKABESIiIiAEFTM1c0ZuHSAIABEyMhIiIiIzABAJAIN1pq6E1dEYE4AZuuNXQmBMAEKmZq5owJQARMjISIiIiMwAgCQCDdcauhNXRGBOAGbrjV0JgTABCpmauaMCQAETIyEiIiIjMAYAkAg3XGroTV0RgTgBmAIauhMCYAIVMzVzRgRgAiZCRERERgDgEGAIauhMCYAIVMzVzRgRAAiZCRERERgCgEGAIauhMCYAIWMCYAE3VAAkZGSmZq5owIgARMjMwGDdaauhMCYAM3WmroQATdaauhNXRAAmrojAlACFTM1c0YEIAImAsYAhq6EwJQAhYwJQATdUACRkZKZmrmjAhABEwETdcauhMCQAIVMzVzRgQAAiYCpuuNXQmBIAELGBIACbqgASMlMzVzRgPGBGACJkZCRmACAGAEbrTV0Jq6IwIwAjAGNXQmBEACLG6oAEjMzMzV0gAJAIEAgSmamAobrAAgBiIVM1ABAIIhUzUAEAoiFTNQAQDCIVM1ABFTNTMzMzV0gBBGRkZmpgRgSGBaAEZgRGYC6QACmagAgIkQqZqACKmamA0AEQmBAJERgAgCAJkQCpmBEZgLpABKZqACAiRCpmoAICZEKmagAipmpgJACEKmamA+AGQmBGJERmAGAKAIAsAqRALmYERmAukAIpmoAICJEKmagAgJkQqZqACAqRCpmoAIqZqYCgAxCpmpgKgCkKmamBEAIQmBMJERmYAQAwAoAgDIDAC5EAyBAQDhgPGBWACbqgCSAZIBkjM1MCECI3WgEgPEA0QDIEJCpmpgMADkKmamAyAMQqZqYDQApCYDwkZmYAIAoAgAYAQCICAB4BxEAgQCBAIAMEZmZmaukABIyUzNXNGA6YEQAImRmZqrnwASASIyMzNVc+ACQChGZmqufNXRABEpmpgEmroQBSEyUzUzMzM1dIACRkZKZmrmjAnABEzM1Vz5gVgBEA2RmZqrnzV0RgWABkZKZqZmZmaukABIyMlMzVzRgXAAiZmaq58wMQAiAhIyMzNVc+ACQEZGRmZqrnwASAlIzM1Vz5q6IAIlM1MCU1dCYG4BBCpmpgTGroQBiFTNTAnNXQgCkJgVmZgWABgBAAgPAOgOEBMBcBaauiACArNXRGBkAGBSKmZq5owLQARMzNVc+YGIARAQkZmaq581dEYGQAZKZqYCxq6EwMwBCEwJTAlABAYICICoCkBYwMQATdUAEQDxAPEA8QDwExCYEBgQAAgJmroTAtAEIBwCQCMVMzVzRgUAAiZmaq58wKwAiMB0BsgGwIwEDArABN1QARAMEAwQDBAMAQEJgNGYCgAYAIBpq6EAQAsgFQHQHDV0QAQDRgQgAgDG6oAIgDyAPIA8gDwFyMzMzNXSAAkZGSmZq5owHgARMzNVc+YEIARAIkZmaq581dEYEQAZKZqYB5q6EwIwBCEwFTARABAIIBIBoBkVMzVzRgOgAiZmaq58wIQAiARIzM1Vz5q6IwIgAyUzUwDzV0JgRgCEJgKmAqACAQQCQDQDIAxgQgAm6oAIgDiAOIA4gDgFiMlMzVzRgNGA+ACJuuNXQmA8ACLG6oAEQChAOESIgAREiIAIRABIiADIhIzABADACIzMzM1dIACQApACkAKQApGAObrgAgDSEiMAEAMjMzMzV0gAJABkAGQAZGAKbrQAiADALEAEiABISIwAgAyIhIjMwAQBQBAAzAPIjMzVXPgAkAIRmAMauhACMAM1dEAEkwgAkQAREJEZgAgCABmAWRCREpmoAQmACAGRCagBESmZq5ozcOAOAEIAImZqYBIBQA4AYAokACJkxgApIEcOiBJbmNvcnJlY3QgbnVtYmVyIG9mIHRva2VucwA3LJIBJHZhbGlkYXRlQ29uZmlndXJhdGlvbiwgdXBncmFkZU1pbnRlcgAiY3DpAAG4dIAI3DpACG4dIAZVc8qudIyMAEAEiMwAzACACABB"

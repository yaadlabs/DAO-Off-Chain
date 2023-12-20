module Test.SampleData where

import Contract.Prelude
import Data.Typelevel.Undefined

import Contract.Value (TokenName, adaToken)
import LambdaBuffers.ApplicationTypes.Configuration (DynamicConfigDatum)

sampleDynamicConfig :: DynamicConfigDatum
sampleDynamicConfig = undefined

sampleNftTokenName :: TokenName
sampleNftTokenName = adaToken

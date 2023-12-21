module Dao.Main (main) where

import Contract.Prelude

import Contract.Config as Contract.Config
import Contract.Monad as Contract.Monad
import Dao.Workflow.CreateConfig (createConfig)
import Test.SampleData (sampleDynamicConfig, sampleNftTokenName)

main :: Effect Unit
main = Contract.Monad.launchAff_
  $ void
  $ Contract.Monad.runContract Contract.Config.testnetNamiConfig
  $ createConfig sampleDynamicConfig sampleNftTokenName
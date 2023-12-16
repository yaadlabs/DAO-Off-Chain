-- | This module is used to serve the E2E tests to the headless browser.
module Dao.Test.E2E.Serve where

import Contract.Prelude

import Contract.Config
  ( ContractParams
  , testnetNamiConfig
  )
import Contract.Monad (Contract)
import Contract.Test.Cip30Mock
  ( WalletMock(MockNami)
  )
import Contract.Test.E2E (E2EConfigName, E2ETestName, addLinks, route)
import Data.Map (Map)
import Data.Map as Map
import Dao.Workflow.CreateConfig (createConfig)

main :: Effect Unit
main = do
  addLinks configs tests
  route configs tests

configs :: Map E2EConfigName (ContractParams /\ Maybe WalletMock)
configs = Map.fromFoldable
  [ "nami" /\ testnetNamiConfig /\ Nothing
  , "nami-mock" /\ testnetNamiConfig /\ Just MockNami
  ]

tests :: Map E2ETestName (Contract Unit)
tests = Map.fromFoldable
  [ "Contract" /\ createConfig
  ]

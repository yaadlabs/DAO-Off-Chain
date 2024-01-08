{-|
Module: Test.Workflow.CreateConfig
Description: Test the create config workflow
-}
module Test.Workflow.CreateConfig (suite) where

import Contract.Prelude (Unit, void, ($))
import Contract.Test.Mote (TestPlanM)
import Contract.Test.Plutip
  ( InitialUTxOs
  , PlutipTest
  , withKeyWallet
  , withWallets
  )
import Dao.Workflow.CreateConfig (createConfig)
import JS.BigInt (fromInt) as BigInt
import Mote (group, test)
import Test.Data.Config (sampleConfigParams)

suite :: TestPlanM PlutipTest Unit
suite = do
  group "DAO tests" do
    test "Create Config test" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 5_000_000
          , BigInt.fromInt 2_000_000_000
          ]
      withWallets distribution \wallet -> do
        withKeyWallet wallet do
          void $ createConfig sampleConfigParams

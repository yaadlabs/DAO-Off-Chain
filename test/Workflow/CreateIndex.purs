{-|
Module: Test.Workflow.CreateIndex
Description: Test the create index workflow
-}
module Test.Workflow.CreateIndex (suite) where

import Contract.Prelude (Unit, void, ($))
import Contract.Test.Mote (TestPlanM)
import Contract.Test.Plutip
  ( InitialUTxOs
  , PlutipTest
  , withKeyWallet
  , withWallets
  )
import Contract.Value (adaToken)
import Dao.Workflow.CreateIndex (createIndex)
import JS.BigInt (fromInt) as BigInt
import Mote (group, test)

suite :: TestPlanM PlutipTest Unit
suite = do
  group "DAO tests" do
    test "Create Index test" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 5_000_000
          , BigInt.fromInt 2_000_000_000
          ]
      withWallets distribution \wallet -> do
        withKeyWallet wallet do
          void $ createIndex adaToken

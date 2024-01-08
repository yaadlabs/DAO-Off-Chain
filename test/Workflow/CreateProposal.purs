{-|
Module: Test.Workflow.CreateProposal
Description: Test the create proposal workflow
-}
module Test.Workflow.CreateProposal (suite) where

import Contract.Prelude (Unit, bind, discard, pure, unit, void, ($), (/\))
import Contract.Test.Mote (TestPlanM)
import Contract.Test.Plutip
  ( InitialUTxOs
  , PlutipTest
  , withKeyWallet
  , withWallets
  )
import Contract.Transaction (awaitTxConfirmedWithTimeout)
import Contract.Value (adaToken)
import Dao.Workflow.CreateConfig (createConfig)
import Dao.Workflow.CreateIndex (createIndex)
import Dao.Workflow.CreateProposal (createProposal)
import Data.Time.Duration (Seconds(Seconds))
import JS.BigInt (fromInt) as BigInt
import Mote (group, test)
import Test.Data.Config (sampleConfigParams)
import Test.Data.Tally (sampleTallyStateDatum)

suite :: TestPlanM PlutipTest Unit
suite = do
  group "DAO tests" do
    test "Create proposal test" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 5_000_000
          , BigInt.fromInt 2_000_000_000
          ]
      withWallets distribution \wallet -> do
        withKeyWallet wallet do
          (createConfigTxHash /\ createConfigSymbol /\ configTokenName) <-
            createConfig sampleConfigParams
          void $ awaitTxConfirmedWithTimeout (Seconds 600.0) createConfigTxHash

          (createIndexTxHash /\ createIndexSymbol /\ indexTokenName) <-
            createIndex adaToken
          void $ awaitTxConfirmedWithTimeout (Seconds 600.0) createIndexTxHash

          sampleTallyStateDatum' <- sampleTallyStateDatum

          -- pure unit
          void $
            createProposal
              createConfigSymbol
              createIndexSymbol
              configTokenName
              indexTokenName
              sampleTallyStateDatum'

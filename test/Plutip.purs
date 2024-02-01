{-|
Module: Test.Plutip
Description: Entry point for Plutip tests
-}
module Test.Plutip (main) where

import Contract.Prelude

import Contract.Test.Mote (interpretWithConfig)
import Contract.Test.Plutip
  ( PlutipConfig
  , testPlutipContracts
  )
import Contract.Test.Utils (exitCode, interruptOnSignal)
import Ctl.Internal.Contract.Hooks (emptyHooks)
import Data.Maybe (Maybe(Just))
import Data.Posix.Signal (Signal(SIGINT))
import Data.Time.Duration (Seconds(Seconds))
import Data.UInt as UInt
import Effect.Aff
  ( Milliseconds(Milliseconds)
  , cancelWith
  , effectCanceler
  , launchAff
  )
import Test.Spec.Runner (defaultConfig)
import Test.Workflow.MultipleVotesWithCancel as MultipleVotesWithCancel
import Test.Workflow.TreasuryTrip as TreasuryTrip

main :: Effect Unit
main = interruptOnSignal SIGINT =<< launchAff do
  flip cancelWith (effectCanceler (exitCode 1)) do
    interpretWithConfig
      defaultConfig { timeout = Just $ Milliseconds 70_000.0, exit = true } $
      testPlutipContracts plutipConfig do
        MultipleVotesWithCancel.suite

-- TreasuryTrip.suite

plutipConfig :: PlutipConfig
plutipConfig =
  { host: "127.0.0.1"
  , port: UInt.fromInt 8082
  , logLevel: Trace
  -- Server configs are used to deploy the corresponding services.
  , ogmiosConfig:
      { port: UInt.fromInt 1338
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , kupoConfig:
      { port: UInt.fromInt 1443
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , suppressLogs: false
  , customLogger: Nothing
  , hooks: emptyHooks
  , clusterConfig:
      { slotLength: Seconds 0.1
      , epochSize: Nothing
      , maxTxSize: Just $ UInt.fromInt 80000
      , raiseExUnitsToMax: true
      }
  }

{-|
Module: Test.Plutip
Description: Entry point for Plutip tests
-}
module Test.Plutip (main) where

import Contract.Prelude

import Contract.Test.Mote (interpretWithConfig)
import Contract.Test.Plutip
  ( defaultPlutipConfig
  , testPlutipContracts
  )
import Contract.Test.Utils (exitCode, interruptOnSignal)
import Data.Posix.Signal (Signal(SIGINT))
import Effect.Aff
  ( Milliseconds(Milliseconds)
  , cancelWith
  , effectCanceler
  , launchAff
  )
import Test.Spec.Runner (defaultConfig)
import Test.Workflow.CreateConfig as CreateConfig
import Test.Workflow.CreateIndex as CreateIndex

main :: Effect Unit
main = interruptOnSignal SIGINT =<< launchAff do
  flip cancelWith (effectCanceler (exitCode 1)) do
    interpretWithConfig
      defaultConfig { timeout = Just $ Milliseconds 70_000.0, exit = true } $
      testPlutipContracts defaultPlutipConfig do
        CreateConfig.suite
        CreateIndex.suite

module Dao.Test.E2E where

import Prelude

import Contract.Test.E2E (parseCliArgs, runE2ECommand)
import Effect (Effect)
import Effect.Aff (launchAff_)

-- Run with `spago test --main Dao.Test.E2E`
main :: Effect Unit
main = parseCliArgs >>= runE2ECommand >>> launchAff_

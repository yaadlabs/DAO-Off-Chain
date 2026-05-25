-- | Some CTL imports needed for 'ContractParams' config 
module Dao.Web.Ctl
  ( -- * Log levels, just use trace for now
    traceLogLevel
  -- * Network id, just need testnet for now
  , testnetId
  , emptyHooks
  ) where

import Cardano.Types (NetworkId(TestnetId))
import Contract.Config (emptyHooks) as Ctl
import Ctl.Internal.Contract.Hooks (Hooks) as Ctl
import Data.Log.Level (LogLevel(Trace))

-- | Testnet network ID
testnetId :: NetworkId
testnetId = TestnetId

-- | Needed for ContractParams
emptyHooks :: Ctl.Hooks
emptyHooks = Ctl.emptyHooks

-- * Log levels, just use trace for now

traceLogLevel :: LogLevel
traceLogLevel = Trace

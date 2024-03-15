-- | Some CTL imports needed for 'ContractParams' config 
module Dao.Web.Ctl
  ( -- * Wallet spec, just use Nami for now
    namiWalletSpec
  -- * Log levels, just use trace for now
  , traceLogLevel
  -- * Network id, just need testnet for now
  , testnetId
  , emptyHooks
  ) where

import Contract.Config
  ( NetworkId(TestnetId)
  , emptyHooks
  ) as Ctl
import Ctl.Internal.Contract.Hooks (Hooks) as Ctl
import Ctl.Internal.Wallet.Spec
  ( WalletSpec(ConnectToNami)
  ) as Ctl
import Data.Log.Level (LogLevel(Trace))

-- | Testnet network ID
testnetId :: Ctl.NetworkId
testnetId = Ctl.TestnetId

-- | Just use Nami wallet for now
namiWalletSpec :: Ctl.WalletSpec
namiWalletSpec = Ctl.ConnectToNami

-- | Needed for ContractParams
emptyHooks :: Ctl.Hooks
emptyHooks = Ctl.emptyHooks

-- * Log levels, just use trace for now

traceLogLevel :: LogLevel
traceLogLevel = Trace

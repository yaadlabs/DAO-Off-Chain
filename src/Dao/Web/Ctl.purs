-- | Some CTL imports needed for 'ContractParams' config 
module Dao.Web.Ctl
  ( -- * Wallet spec, just use Nami for now
    namiWalletSpec
  -- * Log levels, just use trace for now
  , traceLogLevel
  -- * Network id, just need testnet for now
  , testnetId
  , emptyHooks
  , contractConfig
  ) where

import Contract.Config
  ( ContractParams
  , NetworkId(TestnetId)
  , ServerConfig
  , emptyHooks
  , defaultSynchronizationParams
  , defaultTimeParams
  ) as Ctl
import Ctl.Internal.Contract.Hooks (Hooks) as Ctl
import Ctl.Internal.Contract.QueryBackend (mkCtlBackendParams) as Ctl
import Ctl.Internal.Wallet.Spec
  ( WalletSpec(ConnectToNami)
  ) as Ctl
import Data.Log.Level (LogLevel(Trace))
import Data.Maybe (Maybe(Just, Nothing))
import Data.UInt as UInt

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

-- ogmiosConfig :: Ctl.ServerConfig
-- ogmiosConfig =
--   { port: UInt.fromInt 443
--   , host: "ogmios.preview.ctl-runtime.staging.mlabs.city"
--   , secure: true
--   , path: Nothing
--   }

ogmiosConfig :: Ctl.ServerConfig
ogmiosConfig =
  { port: UInt.fromInt 9001
  , host: "localho.st"
  , secure: false
  , path: Nothing
  }

-- kupoConfig :: Ctl.ServerConfig
-- kupoConfig =
--   { port: UInt.fromInt 443
--   , host: "kupo.preview.ctl-runtime.staging.mlabs.city"
--   , secure: true
--   , path: Nothing
--   }

kupoConfig :: Ctl.ServerConfig
kupoConfig =
  { port: UInt.fromInt 9002
  , host: "localho.st"
  , secure: false
  , path: Nothing
  }

contractConfig :: Ctl.ContractParams
contractConfig =
  { backendParams: Ctl.mkCtlBackendParams
    { ogmiosConfig: ogmiosConfig
    , kupoConfig: kupoConfig
    }
  , networkId: testnetId
  , logLevel: traceLogLevel
  , walletSpec: Just namiWalletSpec
  , customLogger: Nothing
  , suppressLogs: false
  , hooks: emptyHooks
  , synchronizationParams: Ctl.defaultSynchronizationParams
  , timeParams: Ctl.defaultTimeParams
  }
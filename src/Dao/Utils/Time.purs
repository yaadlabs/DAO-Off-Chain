{-|
Module: Dao.Utils.Time
Description: Helpers for dealing with time
-}
module Dao.Utils.Time
  ( currentEra
  , mkOnchainTimeRange
  , mkValidityRange
  , mkTimeRangeWithinSummary
  , oneMinute
  ) where

import Contract.Prelude

import Contract.Chain (ChainTip(ChainTip), Tip(Tip), currentTime, getTip)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.Time
  ( OnchainPOSIXTimeRange(OnchainPOSIXTimeRange)
  , POSIXTime(POSIXTime)
  , POSIXTimeRange
  , Slot
  , ToOnChainPosixTimeRangeError
  , getEraSummaries
  , getSystemStart
  , toOnchainPosixTimeRange
  )
import Ctl.Internal.Types.EraSummaries (EraSummary)
import Ctl.Internal.Types.Interval
  ( Interval(FiniteInterval)
  )
import JS.BigInt as BigInt

-- | Make validity range from current time for specified period of time
mkValidityRange :: POSIXTime -> Contract POSIXTimeRange
mkValidityRange timePeriod = do
  currentTime' <- currentTime
  let
    endTime = currentTime' + timePeriod

    timeRange :: POSIXTimeRange
    timeRange = FiniteInterval currentTime' endTime

  mkTimeRangeWithinSummary timeRange

-- | Get current era
currentEra :: Contract EraSummary
currentEra = do
  eraSummaries <- getEraSummaries
  currSlot <- getCurrentSlot
  let
    findSummary era =
      (era # unwrap # _.start # unwrap # _.slot) <= currSlot &&
        case era # unwrap # _.end of
          Just end -> (end # unwrap # _.slot) > currSlot
          Nothing -> true
  logInfo' (show eraSummaries)
  logInfo' (show currSlot)
  liftContractM "Could not find era sumamry" $ find findSummary $ unwrap
    eraSummaries

getCurrentSlot :: Contract Slot
getCurrentSlot = getTip
  >>= (getSlot >>> liftContractM "getSlot failed")
  where
  getSlot (Tip (ChainTip { slot })) = Just slot
  getSlot _ = Nothing

mkTimeRangeWithinSummary ::
  Interval POSIXTime -> Contract (Interval POSIXTime)
mkTimeRangeWithinSummary desiredRange = do
  (desiredStart /\ desiredEnd) <-
    case desiredRange of
      FiniteInterval start end -> pure (start /\ end)
      i -> liftContractM
        ("Could not convert to start-end range: " <> show i)
        Nothing
  era <- currentEra
  let params = unwrap (unwrap era).parameters
  slotLength <- liftContractM "Could not get slot length" $ BigInt.fromNumber $
    unwrap params.slotLength
  let
    offset = unwrap params.safeZone * slotLength
    endTime = desiredStart + POSIXTime offset
    oneSec = POSIXTime $ BigInt.fromInt 1000
    range = FiniteInterval
      (desiredStart + oneSec)
      (min desiredEnd endTime - oneSec)
  logInfo' (show desiredRange)
  logInfo' (show range)
  pure range

mkOnchainTimeRange ::
  POSIXTimeRange ->
  Contract POSIXTimeRange
mkOnchainTimeRange timeRange = do
  logInfo' $ "Converting time range: " <> show timeRange
  onChainTimeRange <-
    liftedM "Failed to make on-chain time range." $ mkOnchainTimeRange'
      timeRange
  pure onChainTimeRange

-- | Should be equivalent to the on-chain value for a time range.
mkOnchainTimeRange' ::
  POSIXTimeRange ->
  Contract (Maybe POSIXTimeRange)
mkOnchainTimeRange' pTime = do
  timeRange <- mkOnchainTimeRange'' pTime
  either
    ( \err -> logInfo' ("Failed to make on-chain time range: " <> show err) $>
        Nothing
    )
    (\(OnchainPOSIXTimeRange pTime') -> pure $ Just pTime')
    timeRange

mkOnchainTimeRange'' ::
  POSIXTimeRange ->
  Contract (Either ToOnChainPosixTimeRangeError OnchainPOSIXTimeRange)
mkOnchainTimeRange'' pTime = do
  eraSummaries <- getEraSummaries
  sysStart <- getSystemStart
  liftEffect $ pure $ toOnchainPosixTimeRange eraSummaries sysStart pTime

oneMinute :: Int
oneMinute = 1000 * 60

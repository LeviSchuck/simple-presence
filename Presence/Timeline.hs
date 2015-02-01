{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Presence.Timeline where

import Data.Map as M
import System.IO.Unsafe
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Data.Foldable as F
import Data.Int
import Data.Text as T
import Data.Monoid
import qualified Data.Aeson as A
import Data.Aeson((.=))

import Presence.Status

type UnixTime = Int64
type UserID = T.Text
type SiteID = T.Text

minute :: UnixTime
minute = 60

hour :: UnixTime
hour = 60 * minute

day :: UnixTime
day = 24 * hour

away :: UnixTime
away = 5 * minute

disconnect :: UnixTime
disconnect = 3 * minute

deviceDisconnect :: UnixTime
deviceDisconnect = 1 * day

data TriBool
      = TriNoInfo
      | TriTrue
      | TriFalse
      deriving(Eq)

data TimelineEvent
      = HeartBeat UnixTime
      | Activity UnixTime
      | EDevice UnixTime
      | MarkBusy
      | UnmarkBusy
      | MarkInvisible
      | UnmarkInvisible

instance Monoid TriBool where
    mempty = TriNoInfo
    mappend TriNoInfo a = a
    mappend a TriNoInfo = a
    mappend _ a         = a

instance A.ToJSON TriBool where
    toJSON TriTrue  = A.toJSON True
    toJSON _        = A.toJSON False


data Timeline = Timeline
    { lastHeartbeat :: UnixTime
    , lastActivity  :: UnixTime
    , lastDevice    :: UnixTime
    , isDevice      :: TriBool
    , isBusy        :: TriBool
    , isInvis       :: TriBool
    }

instance Monoid Timeline where
    mempty = Timeline 0 0 0 mempty mempty mempty
    mappend a b = Timeline
          { lastHeartbeat = (max (lastHeartbeat a) (lastHeartbeat b))
          , lastActivity = (max (lastActivity a) (lastActivity b))
          , lastDevice = (max (lastDevice a) (lastDevice b))
          , isDevice = mappend (isDevice a) (isDevice b)
          , isBusy = mappend (isBusy a) (isBusy b)
          , isInvis = mappend (isInvis a) (isInvis b)
          }

instance A.ToJSON Timeline where
    toJSON Timeline{..} = A.object
        [ "last" .= A.object
          [ "heartbeat" .= lastHeartbeat
          , "activity"  .= lastActivity
          , "device"    .= lastDevice
          ]
        , "device"  .= isDevice
        , "busy"    .= isBusy
        ]

type UserTimelines = M.Map UserID (TVar Timeline)

type SiteTimelines = M.Map SiteID (TVar UserTimelines)

siteTimelines :: TVar SiteTimelines
siteTimelines = unsafePerformIO $ newTVarIO M.empty

interpretStatus :: UnixTime -> Timeline -> Status
interpretStatus t ti@Timeline{..}
    -- Respect invisibility first and foremost
    | isInvis == TriTrue = Offline
    -- If active within the disconnect range
    | lastActivity >= t - disconnect = secondary ti
    -- If no recent heartbeats
    | lastHeartbeat < t - disconnect = tertiary ti
    -- Handle connected status
    | otherwise = secondary ti
    where
      secondary Timeline{..}
        | isBusy == TriTrue = Busy
        -- Activity is within away period
        | lastActivity >= t - away = Online
        | otherwise = Away
      tertiary Timeline{..}
        -- If a device was used within the device-timeout range
        | isDevice == TriTrue && lastDevice >= t - deviceDisconnect = Device
        -- Fall through to offline
        | otherwise = Offline

getTimeline :: (SiteID, UserID) -> STM Timeline
getTimeline (s, u) = do
  sites <- readTVar siteTimelines
  -- Prepare a site if it isn't already available
  userTimelines <- case M.lookup s sites of
      Nothing -> do
          v <- newTVar mempty
          modifyTVar' siteTimelines $ M.insert s v
          return v
      Just x -> return x
  -- Lookup a timeline if available
  -- otherwise return a blank one
  timelines <- readTVar userTimelines
  case M.lookup u timelines of
      Nothing -> return mempty
      Just t -> readTVar t

getStatusIO :: UnixTime -> (SiteID, UserID) -> IO (Status, Timeline)
getStatusIO t su@(s, u) = atomically $ do
    tl <- getTimeline su
    if isInvis tl == TriTrue
    then return (Offline, mempty) -- Leak no information
    else return (interpretStatus t tl, tl)

eventIO :: (SiteID, UserID) -> Timeline -> IO ()
eventIO (s, u) t = atomically $ do
    sites <- readTVar siteTimelines
    -- Prepare a site if it isn't already available
    userTimelines <- case M.lookup s sites of
        Nothing -> do
            v <- newTVar mempty
            modifyTVar' siteTimelines $ M.insert s v
            return v
        Just x -> return x
    timelines <- readTVar userTimelines
    -- Make a timeline if this is the first event
    timeline <- case M.lookup u timelines of
        Nothing -> do
            v <- newTVar mempty
            modifyTVar' userTimelines $ M.insert u v
            return v
        Just x -> return x
    -- Add the event to our timeline
    modifyTVar' timeline $ mappend t

handleEventIO :: (SiteID, UserID) -> TimelineEvent -> IO ()
handleEventIO su (HeartBeat t) = eventIO su $ mempty {lastHeartbeat = t}
handleEventIO su (Activity t) = eventIO su $ mempty {lastActivity = t}
handleEventIO su (EDevice t) = eventIO su $ mempty {lastDevice = t, isDevice = TriTrue}
handleEventIO su MarkBusy = eventIO su $ mempty {isBusy = TriTrue}
handleEventIO su UnmarkBusy = eventIO su $ mempty {isBusy = TriFalse}
handleEventIO su MarkInvisible = eventIO su $ mempty {isInvis = TriTrue}
handleEventIO su UnmarkInvisible = eventIO su $ mempty {isInvis = TriFalse}

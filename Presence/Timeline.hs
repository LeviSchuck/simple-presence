{-# LANGUAGE RecordWildCards #-}

module Presence.Timeline where

import Data.Map as M
import System.IO.Unsafe
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Data.Foldable as F
import Data.Int
import Data.Text as T
import Data.Monoid

import Presence.Status


-- Let's try to keep a 30 minute window.

type UnixTime = Int64
type UserID = T.Text

minute :: UnixTime
minute = 60

away :: UnixTime
away = 5 * minute

disconnect :: UnixTime
disconnect = 3 * minute

data Timeline = Timeline
    { lastHeartbeat :: UnixTime
    , lastActivity  :: UnixTime
    }

instance Monoid Timeline where
    mempty = Timeline 0 0
    mappend a b = Timeline
          { lastHeartbeat = (max (lastHeartbeat a) (lastHeartbeat b))
          , lastActivity = (max (lastActivity a) (lastActivity b))
          }

type UserTimeline = M.Map UserID (TVar Timeline)

userTimelines :: TVar UserTimeline
userTimelines = unsafePerformIO $ newTVarIO M.empty


getStatus :: UnixTime -> UserID -> STM Status
getStatus t u = do
    timelines <- readTVar userTimelines
    case M.lookup u timelines of
        Nothing -> return Offline
        Just timeline -> readTVar timeline >>= return.interp
    where
      interp Timeline{..}
          | lastActivity >= t - disconnect = Online
          | lastHeartbeat < t - disconnect = Offline
          | lastActivity >= t - away = Online
          | otherwise = Away

getStatusIO :: UnixTime -> UserID -> IO Status
getStatusIO t u = atomically $ getStatus t u

eventIO :: UserID -> Timeline -> IO ()
eventIO u t = atomically $ do
    timelines <- readTVar userTimelines
    timeline <- case M.lookup u timelines of
        Nothing -> do
            v <- newTVar mempty
            modifyTVar' userTimelines $ M.insert u v
            return v
        Just x -> return x
    modifyTVar' timeline $ mappend t

heartBeatIO :: UnixTime -> UserID -> IO ()
heartBeatIO t u = eventIO u $ mempty {lastHeartbeat = t}

activityIO :: UnixTime -> UserID -> IO ()
activityIO t u = eventIO u $ mempty {lastActivity = t}

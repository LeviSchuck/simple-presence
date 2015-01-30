{-# LANGUAGE OverloadedStrings #-}

-- | Main entry point to the application.
module Main where

import Web.Scotty

import Control.Monad.IO.Class(liftIO)

import Presence.Timeline
import Presence.Status
import Data.Traversable as T
import Data.Text as T
import Data.UnixTime as U
import Foreign.C.Types
import System.IO.Unsafe
import Data.IORef
import Control.Concurrent
import Data.Int
import Control.Monad

timeRef :: IORef Int64
timeRef = unsafePerformIO $ newIORef 0

maxUserLength :: Int
maxUserLength = 1024

-- | The main entry point.
main :: IO ()
main = do
    _ <- forkIO $ forever $ do
        t <- getUnixTime
        let (CTime t') = utSeconds t
        writeIORef timeRef t'
        threadDelay 1000000 -- Delay 1 second approx
    scotty 30400 $ do
        get "/" $ text "Up and running."
        get "/time" $ do
            time <- getTime
            json time
        get "/heartbeat/:user" $ do
            t <- getTime
            u <- getUser
            liftIO $ heartBeatIO t u
            text "OK"
        get "/activity/:user" $ do
            t <- getTime
            u <- getUser
            liftIO $ activityIO t u
            text "OK"
        get "/status/:user" $ do
            t <- getTime
            u <- getUser
            (status, active) <- liftIO $ getStatusIO t u
            json $ (u, showStatus status, active)
    where
        getTime = rescue (do
            t <- param "time"
            return $ fromInteger t
            ) (\_ -> liftIO $ readIORef timeRef)
        getUser = do
            u <- param "user"
            if T.length u < maxUserLength
            then return u
            else next

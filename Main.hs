{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Main entry point to the application.
module Main where

import Web.Scotty

import Control.Monad.IO.Class(liftIO)

import Presence.Timeline
import Presence.Status
import qualified Data.Traversable as T
import qualified Data.Text as T
import Data.UnixTime as U
import Foreign.C.Types
import System.IO.Unsafe
import Data.IORef
import Control.Concurrent
import Data.Int
import Control.Monad
import qualified Data.Aeson as A
import Data.Aeson((.=))

timeRef :: IORef Int64
timeRef = unsafePerformIO $ newIORef 0

maxUserLength :: Int
maxUserLength = 1024

data TimeEvent = TEHeartBeat | TEActivity | TEDevice
data FlagEvent = FBusy | FInvis

data StatusResponse = StatusResponse UserID Status Timeline

instance A.ToJSON StatusResponse where
    toJSON (StatusResponse u s t) = A.object
        [ "user"    .= u
        , "status"  .= s
        , "meta"    .= t
        ]


-- | The main entry point.
main :: IO ()
main = do
    _ <- forkIO $ forever $ do
        t <- getUnixTime
        let (CTime t') = utSeconds t
        writeIORef timeRef t'
        threadDelay 1000000 -- Delay 1 second approx
    scotty 30400 $ do
        get "/" $ json ("Hi."::T.Text)
        get "/time" $ getTime >>= json
        get "/heartbeat/:site/:user" $ timeEvent TEHeartBeat
        get "/activity/:site/:user" $ timeEvent TEActivity
        get "/device/:site/:user" $ timeEvent TEActivity
        post "/busy/:site/:user" $ flagEvent FBusy
        post "/invis/:site/:user" $ flagEvent FInvis
        get "/status/:site/:user" $ do
            site <- getSite
            t <- getTime
            user <- getUser
            (st, ti) <- liftIO $ getStatusIO t (site, user)
            json $ StatusResponse user st ti
        post "/:site/status" $ do
            site <- getSite
            t <- getTime
            us <- jsonData
            let users = filter filterLen us
            results <- liftIO $ forM users $ \user -> do
                (st, ti) <- getStatusIO t (site, user)
                return $ StatusResponse user st ti
            json results
    where
        timeEvent e = do
            s <- getSite
            u <- getUser
            t <- getTime
            liftIO $ handleEventIO (s, u) $ case e of
                TEHeartBeat -> HeartBeat t
                TEActivity -> Activity t
                TEDevice -> EDevice t
            json True
        flagEvent f = do
            s <- getSite
            u <- getUser
            fv <- jsonData
            liftIO $ handleEventIO (s, u) $ if fv
                then case f of
                    FBusy -> MarkBusy
                    FInvis -> MarkInvisible
                else case f of
                    FBusy -> UnmarkBusy
                    FInvis -> UnmarkInvisible
            json True
        getTime = rescue (do
            t <- param "time"
            return $ fromInteger t
            ) (\_ -> liftIO $ readIORef timeRef)
        filterLen u = let l =T.length u in l > 0 && l < maxUserLength
        getSite = do
            s <- param "site"
            if filterLen s
            then return s
            else next
        getUser = do
            u <- param "user"
            if filterLen u
            then return u
            else next

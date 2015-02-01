{-# LANGUAGE OverloadedStrings #-}
module Presence.Status where


import qualified Data.Aeson as A

data Status = Online | Away | Device | Busy | Offline

showStatus :: Status -> String
showStatus Online   = "online"
showStatus Away     = "away"
showStatus Busy     = "busy"
showStatus Device   = "device"
showStatus Offline  = "offline"

instance A.ToJSON Status where
    toJSON = A.toJSON.showStatus

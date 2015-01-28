module Presence.Status where

data Status = Online | Away | Offline

showStatus :: Status -> String
showStatus Online   = "online"
showStatus Away     = "away"
showStatus Offline  = "offline"

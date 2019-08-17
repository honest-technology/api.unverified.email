module Dispatcher (
    runnableApp
  , testableApp
  ) where

import Network.Wai (Application)
import Web.Scotty (ScottyM, scottyApp, scotty)

import qualified Controller.Mailbox

app' :: ScottyM ()
app' = Controller.Mailbox.controller

testableApp :: IO Application
testableApp = scottyApp app'

runnableApp :: IO ()
runnableApp = scotty 8080 app'

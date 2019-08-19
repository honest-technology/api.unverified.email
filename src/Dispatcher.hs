module Dispatcher (
    runnableApp
  , testableApp
  ) where

import Network.Wai (Application)
import Web.Scotty (ScottyM, scottyApp, scotty)
import Protolude
import qualified Env
import LoadEnv

import qualified Controller.Mailbox

app' :: ScottyM ()
app' = Controller.Mailbox.controller

testableApp :: IO Application
testableApp = scottyApp app'

runnableApp :: IO ()
runnableApp = do
  loadEnv
  port <- Env.port
  scotty (fromIntegral port) app'

module Dispatcher (
    runnableApp
  , testableApp
  ) where

import qualified Env
import           LoadEnv
import           Network.Wai                          (Application)
import           Network.Wai.Middleware.RequestLogger
import           Protolude
import           Web.Scotty                           (ScottyM, scotty, scottyApp, middleware)

import qualified Controller.Mailbox

app' :: ScottyM ()
app' = Controller.Mailbox.controller

testableApp :: IO Application
testableApp =
  ifM Env.debugMode
      (scottyApp $ middleware logStdoutDev >> app')
      (scottyApp app')

runnableApp :: IO ()
runnableApp = do
  loadEnv
  port <- Env.port
  scotty (fromIntegral port) $ middleware logStdout >> app'

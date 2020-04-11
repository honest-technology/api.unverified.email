module Dispatcher (
    runnableApp
  , testableApp
  ) where

import           Control.Concurrent
import qualified Env
import           LoadEnv
import           Network.Wai                          (Application)
import           Network.Wai.Middleware.RequestLogger
import           Protolude
import           Web.Scotty                           (ScottyM, scotty, scottyApp, middleware)
import qualified Network.Wai.Middleware.Prometheus as Prometheus
import qualified Prometheus as Prometheus
import qualified Prometheus.Metric.GHC as Prometheus

import qualified Controller.Mailbox

{-# NOINLINE metricMailboxCreations #-}
metricMailboxCreations :: Prometheus.Counter
metricMailboxCreations
  = Prometheus.unsafeRegister
  $ Prometheus.counter
  $ Prometheus.Info "api_mailbox_creations" "The number of mailboxes created via the api"

app' :: ScottyM ()
app' = Controller.Mailbox.controller metricMailboxCreations

metricsApp :: ScottyM ()
metricsApp = middleware (Prometheus.prometheus Prometheus.def)

testableApp :: IO Application
testableApp =
  ifM Env.debugMode
      (scottyApp $ middleware logStdoutDev >> app')
      (scottyApp app')

runnableApp :: IO ()
runnableApp = do
  _ <- Prometheus.register Prometheus.ghcMetrics
  loadEnv
  port <- Env.port
  prometheusPort <- Env.prometheusPort
  _metricsThreadId <- forkIO $ scotty (fromIntegral prometheusPort) metricsApp
  scotty (fromIntegral port) $ middleware logStdout >> app'

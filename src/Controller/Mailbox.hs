module Controller.Mailbox (
  controller
) where

import           Web.Scotty (ScottyM, get, param)
import qualified Prometheus
import           Protolude hiding (get)

import qualified Action.Create
import qualified Action.Usage
import qualified Action.Receive

controller :: Prometheus.Counter -> ScottyM ()
controller metricMailboxCreations = do
    get "/" Action.Usage.usage
    get "/create" (Action.Create.createMailbox metricMailboxCreations)
    get "/receive/:mailboxId" (param "mailboxId" >>= Action.Receive.receiveMailbox)

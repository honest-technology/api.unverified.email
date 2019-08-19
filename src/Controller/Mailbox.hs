module Controller.Mailbox (
  controller
) where

import Web.Scotty (ScottyM, get, param)
import Protolude hiding (get)

import qualified Action.Create
import qualified Action.Usage
import qualified Action.Receive

controller :: ScottyM ()
controller = do
    get "/" Action.Usage.usage
    get "/create" Action.Create.createMailbox
    get "/receive/:mailboxId" (param "mailboxId" >>= Action.Receive.receiveMailbox)

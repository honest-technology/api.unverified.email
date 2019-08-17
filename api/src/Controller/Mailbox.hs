module Controller.Mailbox (
  controller
) where

import Web.Scotty (ScottyM, get, param)

import qualified Action.New
import qualified Action.Usage
import qualified Action.Receive

controller :: ScottyM ()
controller = do
    get "/" Action.Usage.usage
    get "/create" Action.New.createMailbox
    get "/receive/:mailboxId" (param "mailboxId" >>= Action.Receive.receiveMailbox)

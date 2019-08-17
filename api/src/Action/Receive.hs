module Action.Receive where

import Control.Monad.IO.Class (liftIO)
import Web.Scotty (ActionM, json)
import Data.Text

import Model.Email

receiveMailbox :: Text -> ActionM ()
receiveMailbox mailboxId = liftIO (readMailbox mailboxId) >>= json

readMailbox :: Text -> IO [Email]
readMailbox _mailboxId = return []  
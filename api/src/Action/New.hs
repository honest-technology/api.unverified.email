module Action.New (createMailbox) where

import Data.Text (Text)
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Model.Mailbox
import Data.Time.Clock
import Web.Scotty (ActionM, json)
import Data.UUID
import Data.UUID.V4

createMailbox :: ActionM ()
createMailbox = liftIO mailbox >>= json

mailbox :: IO Mailbox
mailbox = threadDelay 1_000_000
       >> Mailbox <$> getNewMailboxId <*> getCurrentTime

getNewMailboxId :: IO Text
getNewMailboxId = toText <$> nextRandom

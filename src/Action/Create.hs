module Action.Create (createMailbox) where

import Data.Text (Text)
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Model.Mailbox
import Data.Time.Clock
import Web.Scotty (ActionM, json)
import Data.UUID
import Data.UUID.V4
import Protolude

import qualified Env

createMailbox :: ActionM ()
createMailbox = liftIO mailbox >>= json

mailbox :: IO Mailbox
mailbox = dubiousDosPreventionAttempt >> Mailbox <$> getNewMailboxId <*> getCurrentTime
  where
    dubiousDosPreventionAttempt = do
      noDelayEnabled <- Env.featureToggle "FEATURE_NO_DELAY"
      when noDelayEnabled $ threadDelay 1_000_000

getNewMailboxId :: IO Text
getNewMailboxId = toText <$> nextRandom

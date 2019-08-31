module Action.Create (createMailbox) where

import Control.Concurrent (threadDelay)
import Data.Text          (Text)
import Data.Time.Clock
import Data.UUID
import Data.UUID.V4
import Model.Mailbox
import Protolude
import Web.Scotty         (ActionM, json, liftAndCatchIO)

import qualified Env

createMailbox :: ActionM ()
createMailbox = liftAndCatchIO mailbox >>= json

mailbox :: IO Mailbox
mailbox = dubiousDosPreventionAttempt >> Mailbox <$> getNewMailboxId <*> getCurrentTime
  where
    dubiousDosPreventionAttempt = do
      noDelayEnabled <- Env.featureToggle "FEATURE_NO_DELAY"
      when noDelayEnabled $ threadDelay 1_000_000

getNewMailboxId :: IO Text
getNewMailboxId = toText <$> nextRandom

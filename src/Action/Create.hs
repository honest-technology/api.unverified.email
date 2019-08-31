{-# LANGUAGE QuasiQuotes #-}
module Action.Create (createMailbox) where

import Control.Concurrent      (threadDelay)
import Data.String.Interpolate (i)
import Data.Text               (Text)
import Data.Time.Clock
import Data.UUID
import Data.UUID.V4
import Model.Mailbox
import Protolude
import System.Directory        (createDirectoryIfMissing)
import Web.Scotty              (ActionM, json, liftAndCatchIO)

import qualified Env

createMailbox :: ActionM ()
createMailbox = liftAndCatchIO mailbox >>= json

mailbox :: IO Mailbox
mailbox = dubiousDosPreventionAttempt >> Mailbox <$> getNewMailboxId <*> getCurrentTime
  where
    dubiousDosPreventionAttempt = whenM (Env.featureToggle "FEATURE_NO_DELAY") $ threadDelay 1_000_000

getNewMailboxId :: IO Text
getNewMailboxId = do
  maildir <- Env.maildir
  let expectedDir = [i|#{maildir}/expected|]
  createDirectoryIfMissing True expectedDir
  mailboxId <- toText <$> nextRandom
  writeFile [i|#{expectedDir}/#{mailboxId}|] ""
  return mailboxId

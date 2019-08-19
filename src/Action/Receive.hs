{-# LANGUAGE QuasiQuotes #-}
module Action.Receive where

import Control.Monad.IO.Class  (liftIO)
import Control.Retry
import Data.String.Interpolate (i)
import Data.Text
import Protolude
import Web.Scotty              (ActionM, json)

import qualified Env

import Model.Email

receiveMailbox :: Text -> ActionM ()
receiveMailbox mailboxId = liftIO (readMailbox mailboxId) >>= json

readMailbox :: Text -> IO [Email]
readMailbox mailboxId = do
  maildir <- Env.maildir
  noDelayEnabled <- Env.featureToggle "FEATURE_NO_DELAY"
  if noDelayEnabled then
    findEmails [i|#{maildir}/new/|] mailboxId
  else
    retry' $ findEmails [i|#{maildir}/new/|] mailboxId
  where
  retry' = retrying jitterRetry considerRetrying . const
  jitterRetry = fullJitterBackoff 50_000 <> limitRetries 10
  considerRetrying = const $ return . Protolude.null

findEmails :: FilePath -> Text -> IO [Email]
findEmails _maildir _mailboxId = return []

{-# LANGUAGE QuasiQuotes #-}
module Action.Receive where

import           Control.Monad.IO.Class  (liftIO)
import           Control.Retry
import           Data.String.Interpolate (i)
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.IO            as T
import           Protolude
import           System.Directory        (doesDirectoryExist, listDirectory)
import           Web.Scotty              (ActionM, json)

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
  considerRetrying _ emails = return (null emails)

findEmails :: FilePath -> Text -> IO [Email]
findEmails maildir mailboxId =
  ifM (doesDirectoryExist maildir)
      (do
        mailFiles <- listDirectory maildir >>= mapM (T.readFile . ((maildir++"/")++))
        return . mapMaybe parseEmail $ filter (isForMailboxId mailboxId) mailFiles
        )
      (return [])
  where
  isForMailboxId = T.isInfixOf

{-# LANGUAGE QuasiQuotes #-}
module Action.Receive where

import           Control.Retry
import qualified Data.ByteString          as B
import           Data.String.Interpolate  (i)
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import qualified Data.Text.Encoding.Error as TE
import           Protolude
import           System.Directory         (doesDirectoryExist, listDirectory)
import           Web.Scotty               (ActionM, json, liftAndCatchIO)

import qualified Env

import Model.Email

receiveMailbox :: Text -> ActionM ()
receiveMailbox mailboxId = liftAndCatchIO (readMailbox mailboxId) >>= json

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
  jitterRetry = limitRetriesByCumulativeDelay 28_000_000 (fullJitterBackoff 250_000)
  considerRetrying _ emails = return (null emails)

findEmails :: FilePath -> Text -> IO [Email]
findEmails maildir mailboxId =
  ifM (doesDirectoryExist maildir)
      (do
        mailFiles <- listDirectory maildir >>= mapM (maybeReadFile . ((maildir++"/")++))
        return . mapMaybe parseEmail $ filter (isForMailboxId mailboxId) mailFiles
        )
      (return [])
  where
  isForMailboxId = T.isInfixOf
  maybeReadFile path = TE.decodeUtf8With TE.lenientDecode <$> B.readFile path

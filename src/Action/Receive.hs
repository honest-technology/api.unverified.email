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
import           Text.StringConvert       (s)
import           Web.Scotty               (ActionM, json, liftAndCatchIO)

import qualified Env
import           Model.Email

receiveMailbox :: Text -> ActionM ()
receiveMailbox mailboxId = liftAndCatchIO (readMailbox mailboxId) >>= json

readMailbox :: Text -> IO [Email]
readMailbox mailboxId = do
  maildir <- Env.maildir
  ifM (Env.featureToggle "FEATURE_NO_DELAY")
      (findEmails maildir mailboxId)
      (retry' $ findEmails maildir mailboxId)
  where
  retry' = retrying jitterRetry considerRetrying . const
  jitterRetry = limitRetriesByCumulativeDelay 29_250_000 (fullJitterBackoff 250_000)
  considerRetrying _ emails = return (null emails)

findEmails :: FilePath -> Text -> IO [Email]
findEmails maildir mailboxId =
  ifM (directoriesExist &&^ isMailboxIdExpected mailboxId)
      (do
        mailFiles <- listDirectory newMailsDir >>= mapM (maybeReadFile . ([i|#{newMailsDir}/|]++))
        return . mapMaybe parseEmail $ filter (isForMailboxId mailboxId) mailFiles
        )
      (return [])
  where
  newMailsDir = [i|#{maildir}/new|]
  expectedMailsDir = [i|#{maildir}/expected|]
  isForMailboxId = T.isInfixOf
  maybeReadFile path = TE.decodeUtf8With TE.lenientDecode <$> B.readFile path
  directoriesExist =
    doesDirectoryExist maildir
    &&^ doesDirectoryExist newMailsDir
    &&^ doesDirectoryExist expectedMailsDir
  isMailboxIdExpected mailboxId' =
    fmap (elem (s mailboxId')) (listDirectory expectedMailsDir)

module Controller.ReceiveTest where

import Data.Aeson                     (FromJSON)
import Data.String.Interpolate        (i)
import Data.Text                      (Text)
import Data.Time.Clock                (diffUTCTime, getCurrentTime)
import GHC.Generics
import Lens.Micro
import Lens.Micro.Aeson
import Network.Wai                    (Application)
import Network.Wai.Test
import System.Directory               (createDirectoryIfMissing)
import Test.Hspec                     (SpecWith, describe, it)
import Test.Hspec.Expectations.Lifted
import Test.Hspec.Wai
import Text.StringConvert             (s)

import qualified Env

import Test.Hspec.Wai.FeatureToggle
import Test.Hspec.Wai.ValueMatchers

spec :: SpecWith Application
spec = do
  withFeatureToggleOn "FEATURE_NO_DELAY" $ describe "GET /receive" $ do
    it "responds with empty emails for unknown mailbox id" $
      get "/receive/unknown-mailbox-id" `shouldRespondWith` jsonEmailsForReceive []
    it "responds with an email when it arrives in the mailbox" $ do
      (mailboxId, mailboxAddress) <- createMailbox
      let testEmail = testEmailTo mailboxAddress
      sendEmail testEmail
      get [i|/receive/#{mailboxId}|] `shouldRespondWith` jsonEmailsForReceive [(mailboxAddress, (plainEmail testEmail))]
    it "doesnt show emails for inboxes that were not created" $ do
      pendingWith "Not implemented yet"
      let mailboxId = "guessable-mailbox-id" :: Text
      sendEmail $ testEmailTo [i|#{mailboxId}@unverified.email|]
      get [i|/receive/#{mailboxId}|] `shouldRespondWith` jsonEmailsForReceive []
  describe "GET /receive" $
    it "responds with a delay waiting for an email to arrive" $ do
      timeStart <- liftIO getCurrentTime
      _ <- get "/receive/unknown-mailbox-id"
      timeFinished <- liftIO getCurrentTime
      (timeFinished `diffUTCTime` timeStart) `shouldSatisfy` (> 15)
      (timeFinished `diffUTCTime` timeStart) `shouldSatisfy` (< 29)

jsonEmailsForReceive :: [(Text, Text)] -> ResponseMatcher
jsonEmailsForReceive vs = 200 { matchBody =
  introspectBody
    (\body ->
       [ (body ^.. values . key "address-to" . _String) `equalsTo` map fst vs
       , (body ^.. values . key "full-content" . _String) `equalsTo` map snd vs
       ])
  }

createMailbox :: WaiSession (Text, Text)
createMailbox = do
  mailbox :: Text <- s . simpleBody <$> get "/create"
  let mailboxId = mailbox ^. key "mailbox-id" . _String
  let mailboxAddress = mailbox ^. key "mailbox" . _String
  return (mailboxId, mailboxAddress)

sendEmail :: TestEmail -> WaiSession ()
sendEmail te = do
  maildir <- Env.maildir
  liftIO $ do
    createDirectoryIfMissing True [i|#{maildir}/new/|]
    writeFile [i|#{maildir}/new/1566204945.111.51ff45ed527f|] (s (plainEmail te))

testEmailTo :: Text -> TestEmail
testEmailTo addressTo = TestEmail "test@unverified.email" addressTo "A test email" "Body of a test email"

plainEmail :: TestEmail -> Text
plainEmail TestEmail{..} = [i|
Return-Path: #{addressFrom}
Delivered-To: #{addressTo}
Received: from user-PC (8ta-246-224-193.telkomadsl.co.za [41.246.224.193])
	by 51ff45ed527f (OpenSMTPD) with ESMTP id 116053ef
	for <#{addressTo}>;
	Mon, 19 Aug 2019 08:55:44 +0000 (UTC)
From: #{addressFrom}
Subject: #{subject}
To: #{addressTo}
Date: Mon, 19 Aug 2019 10:55:37 +0200
X-Priority: 3
X-Library: Indy 8.0.25

#{body}
|]

data TestEmail =
  TestEmail
    { addressFrom :: Text
    , addressTo   :: Text
    , subject     :: Text
    , body        :: Text
    }
  deriving (Eq, Show, Generic)

instance FromJSON TestEmail

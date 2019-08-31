module Controller.UsageTest where

import Data.String.Interpolate (i)
import Data.Text
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai

import qualified Env

spec :: SpecWith Application
spec =
  describe "GET /" $ do
    it "responds with usage" $ do
      apiUrl <- Env.apiURL
      infoPageUrl <- Env.infoPageURL
      get "/" `shouldRespondWith` usage apiUrl infoPageUrl
    it "has 'Content-Type: text/plain; charset=utf-8'" $
      get "/" `shouldRespondWith` 200 {matchHeaders = ["Content-Type" <:> "text/plain; charset=utf-8"]}

usage :: Text -> Text -> ResponseMatcher
usage apiUrl infoPageUrl = [i|
This service helps automating email testing.

To create a mailbox issue a HTTP GET request to: https://#{apiUrl}/create
The responce will be a json containing all the information about a newly
created mailbox.

Your test can now send emails to the email address specified in the create
response

Verify them by issuing another HTTP GET request to:
https://#{apiUrl}/receive/<mailbox_id>
replacing <mailbox_id> with the id from the first request. The service will
answer with a list of all emails it can find in mailbox.
The receive command will wait for emails to arrive for at least 15 seconds.

For more details, visit #{infoPageUrl}
|]

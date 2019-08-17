module Main (main) where

import Data.String.Interpolate (i)
import Lens.Micro
import Lens.Micro.Aeson
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JsonMatchers

import Dispatcher (testableApp)

main :: IO ()
main = hspec spec

spec :: Spec
spec = with testableApp $ do
  describe "GET /" $ do
    it "responds with usage" $
      get "/" `shouldRespondWith` usage

    it "has 'Content-Type: text/plain; charset=utf-8'" $
      get "/" `shouldRespondWith` 200 {matchHeaders = ["Content-Type" <:> "text/plain; charset=utf-8"]}

  describe "GET /create" $
    it "responds with information about a new mailbox" $
      get "/create" `shouldRespondWith` jsonResponseForCreate

  describe "GET /receive" $
    it "responds with empty emails for unknown mailbox id" $
      get "/receive/unknown-id" `shouldRespondWith` jsonEmptyResponseForReceive

usage :: ResponseMatcher
usage = [i|
This is a service for testing emails in an automated way.

You can create a mailbox by issuing a GET request to:
https://api.unverified.email/create
The responce will be a json containing all the information about a
newly created mailbox.

After sending emails to the email address specified in the previous
response, you can verify them by issuing another HTTP GET request to
https://api.unverified.email/receive/<mailbox_id>
replacing <mailbox_id> with the id from the first request. The service
will answer with a list of all emails it can find in the mailbox

For more information, please refer to https://unverified.email

Created and hosted by Pavlo Kerestey
|]

jsonResponseForCreate :: ResponseMatcher
jsonResponseForCreate = 200 { matchBody = introspectBody (\body ->
  [ (isUUID (body ^? key "mailbox-id" . _String))
  , ((body ^? key "mailbox" . _String) `contains` "unverified.email")
  , ((body ^? key "receive" . _String) `contains` "https://api.unverified.email/receive")
  , ((body ^? key "created" . _String) `hasLength` 24)
  ])}

jsonEmptyResponseForReceive :: ResponseMatcher
jsonEmptyResponseForReceive = 200 {matchBody = introspectBody (\body ->
  [ (if (body ^.. values) == [] then Nothing else Just [i|#{body} was not []|])
  ])}

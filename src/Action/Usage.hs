{-# LANGUAGE QuasiQuotes #-}
module Action.Usage (usage) where

import Data.Text as T
import Data.Text.Lazy as TL
import Data.String.Interpolate (i)
import Web.Scotty (ActionM, text)

import qualified Env

usage :: ActionM ()
usage = do
  apiUrl <- Env.apiURL
  infoPageUrl <- Env.infoPageURL
  text (usage' apiUrl infoPageUrl)

usage' :: T.Text -> T.Text -> TL.Text
usage' apiUrl infoPageUrl = [i|
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
|]

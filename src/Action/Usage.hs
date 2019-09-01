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

                            UNVERIFIED.EMAIL
                 a tool that helps testing your emails.

# Usage

You can use api.unverified.email to create a mailbox, send email(s) to it
from your tests, and, verify that the email(s) have all the needed content
by downloading them through the api.

1. To create a mailbox, make a GET request to:
   https://#{apiUrl}/create
   The response will be a json containing all the information about a newly
   created mailbox.

2. Send your emails to the mailbox address that you will find from the create
   step.

3. Verify the emails in your test, by making another GEt request to:
   https://#{apiUrl}/receive/<mailbox_id>
   where <mailbox_id> is the one from the creation step.
   The creation step actually includes the receive url, so you don't need
   to concatenate anything in your code.
   unverified.email will answer with a list of all the emails, including the
   To, From, and Subject fields, as well as the raw source of theemail body
   for you to parse :)

# There are few notes though:

- You can create as many mailboxes as you want. The mailboxes and all the
  emails will be deleted 5 minutes after they are created.

- The /receive endpoint will wait for emails to be delivered for at least 15
  seconds, so you should not need to hit the url repeatedly.

- The code is available at https://github.com/ptek/api.unverified.email

*And a note of warning*: unverified.email is not yet finalised. I am certain
it can be used by developers and in your pipelines, and we can iron out the
small kinks together if they appear. But it is still very fresh, so just
open an issue please, if something needs attention.

For more details, visit #{infoPageUrl}
|]

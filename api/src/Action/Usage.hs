{-# LANGUAGE QuasiQuotes #-}
module Action.Usage (usage) where

import Data.String.Interpolate (i)
import Web.Scotty (ActionM, text)

usage :: ActionM ()
usage = text [i|
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

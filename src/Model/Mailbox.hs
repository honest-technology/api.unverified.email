{-# LANGUAGE QuasiQuotes #-}
module Model.Mailbox where

import Data.Aeson
import Data.Text
import Data.Time
import Data.String.Interpolate (i)
import Data.Time.ISO8601 (formatISO8601Millis)

newtype EmailAddress = EmailAddress Text deriving ToJSON
newtype Url = Url Text deriving ToJSON

data Mailbox = Mailbox {
  mailboxId :: Text
, created :: UTCTime
}

receiveUrl :: Mailbox -> Url
receiveUrl m = Url [i|https://api.unverified.email/receive/#{mailboxId m}|]

mailboxAddress :: Mailbox -> EmailAddress
mailboxAddress m = EmailAddress [i|#{mailboxId m}@unverified.email|]

instance ToJSON Mailbox where
  toJSON m = object [
      "mailbox" .= mailboxAddress m
    , "receive" .= receiveUrl m
    , "mailbox_id" .= mailboxId m
    , "created" .= formatISO8601Millis (created m)
    ]

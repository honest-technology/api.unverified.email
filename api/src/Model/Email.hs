module Model.Email where

import Data.Aeson
import Data.Text
import GHC.Generics

data Email = Email {
    to :: Text
  , from :: Text
  , subject :: Text
  , textBody :: Maybe Text
  , htmlBody :: Maybe Text
  } deriving (Generic, Show)

instance ToJSON Email where
  toEncoding = genericToEncoding defaultOptions

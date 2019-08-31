module Model.Email where

import           Data.Aeson
import           Data.Text    (Text)
import qualified Data.Text    as T
import           GHC.Generics
import           Protolude

data Email = Email {
    addressTo   :: Text
  , addressFrom :: Text
  , subject     :: Text
  , fullContent :: Text
  } deriving (Generic, Show)

instance ToJSON Email where
  toEncoding = genericToEncoding (defaultOptions {fieldLabelModifier = camelTo2 '_'})

parseEmail :: Text -> Maybe Email
parseEmail email =
    Email
      <$> parseTo
      <*> parseFrom
      <*> parseSubject
      <*> pure email
  where
  emailLines = T.lines email
  parseTo = T.drop 4 <$> headMay (filter ("To: " `T.isPrefixOf`) emailLines)
  parseFrom = T.drop 6 <$> headMay (filter ("From: " `T.isPrefixOf`) emailLines)
  parseSubject = T.drop 9 <$> headMay (filter ("Subject: " `T.isPrefixOf`) emailLines)

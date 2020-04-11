module Env
  ( port
  , prometheusPort
  , apiURL
  , infoPageURL
  , smtpURL
  , maildir
  , featureToggle
  , debugMode
  ) where

import           Control.Monad.Fail
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Prelude                hiding (fail)
import           System.Environment     (lookupEnv)
import           Text.Read              (readEither)

port :: (MonadIO m, MonadFail m) => m Int
port = readEnv "PORT"

prometheusPort :: (MonadIO m, MonadFail m) => m Int
prometheusPort = readEnv "PORT_PROMETHEUS"

apiURL :: (MonadIO m, MonadFail m) => m Text
apiURL = T.pack <$> readEnvString "API_URL"

smtpURL :: (MonadIO m, MonadFail m) => m Text
smtpURL = T.pack <$> readEnvString "SMTP_URL"

infoPageURL :: (MonadIO m, MonadFail m) => m Text
infoPageURL = T.pack <$> readEnvString "INFO_PAGE_URL"

maildir :: (MonadIO m, MonadFail m) => m FilePath
maildir = readEnvString "MAILDIR"

debugMode :: (MonadIO m) => m Bool
debugMode = featureToggle "DEBUG_MODE"

{-# INLINEABLE featureToggle #-}
featureToggle :: (MonadIO m) => String -> m Bool
featureToggle toggleName = liftIO (parseDevelopmentMode <$> lookupEnv toggleName)
  where
  parseDevelopmentMode (Just "True") = True
  parseDevelopmentMode _otherwise    = False

{-# INLINEABLE readEnvString #-}
readEnvString :: (MonadIO m, MonadFail m) => String -> m String
readEnvString name = do
  result <- liftIO $ lookupEnv name
  case result of
    Nothing -> fail $ "No value set for environment variable: " <> name
    Just x  -> return x

{-# INLINEABLE readEnv #-}
readEnv :: (MonadIO m, MonadFail m, Read a) => String -> m a
readEnv name = do
  val <- readEnvString name
  case readEither val of
    Left err -> fail $ "Error parsing value from environment variable. " <> name <> " = " <> show val <> " => " <> err
    Right result -> return result

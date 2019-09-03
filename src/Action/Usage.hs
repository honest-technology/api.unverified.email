module Action.Usage (usage) where

import Protolude
import Web.Scotty              (ActionM, text)

import qualified Env
import qualified Model.Usage

usage :: ActionM ()
usage = Model.Usage.plainText <$> Env.apiURL <*> Env.infoPageURL <*> Env.smtpURL >>= text

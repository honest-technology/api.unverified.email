module Controller.CreateTest where

import Lens.Micro
import Lens.Micro.Aeson
import Network.Wai      (Application)
import Protolude        (whenM)
import System.Directory (doesDirectoryExist, removeDirectoryRecursive)
import Test.Hspec
import Test.Hspec.Wai


import qualified Env
import           Test.Hspec.Wai.FeatureToggle
import           Test.Hspec.Wai.ValueMatchers


spec :: SpecWith Application
spec = before_ cleanMaildir $
  withFeatureToggleOn "FEATURE_NO_DELAY" $
    describe "GET /create" $
      it "responds with information about a new mailbox" $
       get "/create" `shouldRespondWith` jsonResponseForCreate

jsonResponseForCreate :: ResponseMatcher
jsonResponseForCreate = 200 { matchBody =
  introspectBody
    (\body ->
       [ isUUID (body ^? key "mailbox_id" . _String)
       , (body ^? key "mailbox" . _String) `contains` "@unverified.email"
       , (body ^? key "receive" . _String) `contains` "https://api.unverified.email/receive"
       , (body ^? key "created" . _String) `hasLength` 24
       ])
  }

cleanMaildir :: IO ()
cleanMaildir = do
  maildir <- Env.maildir
  whenM (doesDirectoryExist maildir) (removeDirectoryRecursive maildir)

module Main (main) where

import LoadEnv
import Test.Hspec
import Test.Hspec.Wai

import Dispatcher (testableApp)

import qualified Controller.CreateTest
import qualified Controller.ReceiveTest
import qualified Controller.UsageTest

main :: IO ()
main = do
  loadEnv
  hspec spec

spec :: Spec
spec = with testableApp $ do
  Controller.UsageTest.spec
  Controller.CreateTest.spec
  Controller.ReceiveTest.spec

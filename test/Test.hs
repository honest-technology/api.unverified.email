module Main (main) where

import Test.Hspec
import Test.Hspec.Wai
import LoadEnv

import Dispatcher (testableApp)

import qualified Controller.UsageTest
import qualified Controller.CreateTest
import qualified Controller.ReceiveTest

main :: IO ()
main = do
  loadEnv
  hspec spec

spec :: Spec
spec = with testableApp $ do
  Controller.UsageTest.spec
  Controller.CreateTest.spec
  Controller.ReceiveTest.spec

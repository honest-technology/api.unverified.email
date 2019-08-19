{-# LANGUAGE FlexibleContexts #-}

module Test.Hspec.Wai.FeatureToggle where

import Control.Exception  (bracket_)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Test.Hspec         (SpecWith, around_)

withFeatureToggleOn :: String -> SpecWith a -> SpecWith a
withFeatureToggleOn featureName = around_ (\action -> do
    previousVal <- lookupEnv featureName
    bracket_ set' (unsetTo' previousVal) action
    )
  where
    set' = setEnv featureName "True"
    unsetTo' = \case
        Just v -> setEnv featureName v
        Nothing -> unsetEnv featureName

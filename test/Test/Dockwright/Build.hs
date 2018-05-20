{-# LANGUAGE OverloadedLabels #-}

module Test.Dockwright.Build where

import           RIO
import           Test

import           Dockwright.Build
import           Language.Docker
import           Test.Tasty
import           Test.Tasty.Hspec

test_buildBaseImage :: IO TestTree
test_buildBaseImage = testSpec "buildBaseImage :: RIO Env Dockerfile" $
  context "when Env is testEnv" $
    it "correct case" $
      runRIO testEnv buildBaseImage `shouldReturn` toDockerfile (from $ "debian" `tagged` "latest")

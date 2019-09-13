module Main where

import           RIO
import qualified Test.Dockwright.Build
import qualified Test.Dockwright.Data.Config
import qualified Test.Dockwright.Fetch.Tags

import           Test.Tasty

main :: IO ()
main = defaultMain =<< testGroup "scrapbook package" <$> sequence
  [ Test.Dockwright.Build.tests
  , pure Test.Dockwright.Fetch.Tags.tests
  , pure Test.Dockwright.Data.Config.tests
  ]

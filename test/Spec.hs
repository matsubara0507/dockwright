module Main where

import           RIO
import qualified Test.Dockwright.Build

import           Test.Tasty

main :: IO ()
main = defaultMain =<< testGroup "scrapbook package" <$> sequence
  [ Test.Dockwright.Build.tests
  ]

module Test.Dockwright.Fetch.Tags where

import           RIO

import qualified Data.Aeson                as J
import           Data.Extensible
import           Dockwright.Data.DockerHub (toTag)
import           Dockwright.Fetch.Tags
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Dockwright.Tags"
  [ testGroup "lookupKey"
      [ testCase "exist key" $
          lookupKey ["hoge1", "fuga2"] testJson @?= Just (toTag "abc")
      , testCase "not exist key" $
          lookupKey ["hoge4"] testJson @?= Nothing
      , testCase "empty key" $
          lookupKey [] testJson @?= Nothing
      ]
  ]

testJson :: J.Value
testJson = J.toJSON
    $ #hoge1 @= hoge1
   <: #hoge2 @= (100 :: Int)
   <: #hoge3 @= (Just True :: Maybe Bool)
   <: emptyRecord
   where
     hoge1 :: Record '[ "fuga1" >: Bool, "fuga2" >: Text ]
     hoge1 = #fuga1 @= True <: #fuga2 @= "abc" <: nil

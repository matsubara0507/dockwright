module Test.Dockwright.Data.Config where

import           RIO

import           Data.Extensible
import           Dockwright.Data.Config
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Dockwright.Data.Config"
  [ testGroup "toValTagsConfig"
      [ testCase "shrink" $
          toValTagsConfig testTagsConfig1 @?= testValTagsConfig1
      ]
  , testGroup "toRefTagsConfig"
      [ testCase "when ref is Nothing" $
          toRefTagsConfig testTagsConfig1 @?= testRefTagsConfig1
      , testCase "when ref is Just" $
          toRefTagsConfig testTagsConfig2 @?= testRefTagsConfig2
      ]
  , testGroup "splitOn"
      [ testCase "include sep char" $
          splitOn '/' "ab/cd/ef" @?= ("ab", "cd/ef")
      , testCase "not include sep char" $
          splitOn '/' "ab" @?= ("ab", "")
      ]
  ]

testTagsConfig1, testTagsConfig2, testTagsConfig3 :: TagsConfig
testTagsConfig1
    = #type   @= "value"
   <: #ref    @= Nothing
   <: #keys   @= [ "latest", "develop" ]
   <: #always @= Just True
   <: nil
testTagsConfig2
    = #type  @= "refer"
   <: #ref   @= Just "https://example.com"
   <: #keys   @= [ "v1", "v2" ]
   <: #always @= Just False
   <: nil
testTagsConfig3
    = #type  @= "hoge"
   <: #ref   @= Nothing
   <: #keys   @= []
   <: #always @= Nothing
   <: nil

testValTagsConfig1 :: ValTagsConfig
testValTagsConfig1
    = #keys @= [ "latest", "develop" ]
   <: nil

testRefTagsConfig1, testRefTagsConfig2 :: RefTagsConfig
testRefTagsConfig1
    = #ref @= ""
   <: #keys @= [ "latest", "develop" ]
   <: nil
testRefTagsConfig2
    = #ref @= "https://example.com"
   <: #keys @= [ "v1", "v2" ]
   <: nil

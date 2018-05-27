{-# LANGUAGE OverloadedLabels #-}

module Test where

import           RIO
import qualified RIO.Map                as Map

import           Data.Extensible
import           Dockwright.Data.Config
import           Dockwright.Data.Env

testConfig :: Config
testConfig
    = #output   @= "."
   <: #template @= "./Dockerfile.template"
   <: #base     @= (#repo @= "debian" <: #tag @= "latest" <: nil)
   <: #env      @= Map.fromList [("test1", testDockVal)]
   <: nil

testDockVal :: DockVal
testDockVal
    = #github @= Just (#repo @= "hoge/fuga" <: #hook @= "release" <: nil)
   <: #value  @= Just "1.0.0"
   <: nil

testEnv :: Env
testEnv
    = #config @= testConfig
   <: #logger @= mkLogFunc (\_ _ _ _ -> pure ())
   <: nil
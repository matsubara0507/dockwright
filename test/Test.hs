module Test where

import           RIO
import qualified RIO.Map                as Map

import           Data.Extensible
import           Dockwright.Data.Config
import           Dockwright.Data.Env

testConfig :: Config
testConfig
    = #image    @= "matsubara0507/dockwright"
   <: #output   @= "."
   <: #template @= template
   <: #base     @= (#repo @= "debian" <: #tag @= "latest" <: nil)
   <: #env      @= Just (Map.fromList [("test1", testDockVal)])
   <: #tags     @= []
   <: nil

template :: DockerfileTeamplate
template
    = #before_env @= Nothing
   <: #after_env  @= Just "./Dockerfile.template"
   <: nil

testDockVal :: DockVal
testDockVal
    = #github @= Just (#repo @= "hoge/fuga" <: #hook @= "release" <: #strip_prefix @= Nothing <: nil)
   <: #value  @= Just "1.0.0"
   <: nil

testEnv :: Env
testEnv
    = #config @= testConfig
   <: #logger @= mkLogFunc (\_ _ _ _ -> pure ())
   <: nil

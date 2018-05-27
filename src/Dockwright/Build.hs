{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}

module Dockwright.Build
  ( build
  , buildBaseImage
  , buildDockerEnv
  , readTemplateDockerFile
  ) where

import           RIO
import qualified RIO.Char            as C
import qualified RIO.Map             as Map
import qualified RIO.Text            as Text

import           Dockwright.Data.Env (DockwrightException (..), Env)
import           Dockwright.Fetch    (fetchEnvVal)
import           Language.Docker

build :: RIO Env Dockerfile
build = do
  baseImage  <- buildBaseImage
  dockerEnv  <- buildDockerEnv
  template   <- readTemplateDockerFile
  pure $ baseImage <> dockerEnv <> template

buildBaseImage :: RIO Env Dockerfile
buildBaseImage = do
  logDebug "build base image from config yaml."
  conf <- asks (view #base . view #config)
  pure . toDockerfile $
    from $ tagged (Text.unpack $ conf ^. #repo) (Text.unpack $ conf ^. #tag)

buildDockerEnv :: RIO Env Dockerfile
buildDockerEnv = do
  logDebug "build appending env from config yaml."
  conf <- asks (view #env . view #config)
  toDockerfile . env . Map.toList .
    Map.mapKeys (map C.toUpper) <$> mapWithKeyM fetchEnvVal conf

readTemplateDockerFile :: RIO Env Dockerfile
readTemplateDockerFile = do
  path <- asks (view #template . view #config)
  logDebug (displayShow $ "read template: " <> path)
  (parseString . Text.unpack <$> readFileUtf8 path) >>= \case
    Left err   -> throwM $ DockerfileParseError err
    Right file -> pure file

mapWithKeyM ::  Monad m => (k -> a -> m b) -> Map k a -> m (Map k b)
mapWithKeyM f = sequence . Map.mapWithKey f

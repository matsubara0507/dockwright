{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}

module Dockwright.Build where

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
  dockerEnv <- buildDockerEnv
  template   <- readTemplateDockerFile
  pure $ baseImage <> dockerEnv <> template

buildBaseImage :: RIO Env Dockerfile
buildBaseImage = do
  conf <- asks (view #base . view #config)
  pure . toDockerfile $
    from $ tagged (Text.unpack $ conf ^. #repo) (Text.unpack $ conf ^. #tag)

buildDockerEnv :: RIO Env Dockerfile
buildDockerEnv = do
  conf <- asks (view #env . view #config)
  toDockerfile . env .
    Map.toList . Map.mapKeys (map C.toUpper) <$> mapM fetchEnvVal conf

readTemplateDockerFile :: RIO Env Dockerfile
readTemplateDockerFile = do
  filepath <- asks (view #template . view #config)
  (parseString . Text.unpack <$> readFileUtf8 filepath) >>= \case
    Left err   -> throwM $ DockerfileParseError err
    Right file -> pure file

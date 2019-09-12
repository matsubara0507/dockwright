{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}

module Dockwright.Build
  ( build
  , buildBaseImage
  , buildDockerEnv
  , readTemplateDockerFile
  ) where

import           RIO
import qualified RIO.Map             as Map
import qualified RIO.Text            as Text

import           Data.Fallible
import           Dockwright.Data.Env
import           Dockwright.Fetch    (fetchEnvVal)
import           Language.Docker     (Dockerfile)
import qualified Language.Docker     as Docker

build :: RIO Env (Either BuildError Dockerfile)
build = evalContT $ do
  baseImage  <- lift buildBaseImage
  beforeTepl <- lift readBeforeTemplateDockerFile !?= exit'
  dockerEnv  <- lift buildDockerEnv !?= exit'
  afterTmpl  <- lift readAftreTemplateDockerFile !?= exit'
  pure $ Right (baseImage <> beforeTepl <> dockerEnv <> afterTmpl)
  where
    exit' = exit . pure . Left

buildBaseImage :: RIO Env Dockerfile
buildBaseImage = do
  logDebug "build base image from config yaml."
  conf <- asks (view #base . view #config)
  let tag = Docker.tagged (fromText $ conf ^. #repo) (fromText $ conf ^. #tag)
  pure $ Docker.toDockerfile (Docker.from tag)

buildDockerEnv :: RIO Env (Either BuildError Dockerfile)
buildDockerEnv = do
  logDebug "build appending env from config yaml."
  conf <- asks (view #env . view #config)
  env  <- evalContT $ pure <$> mapWithKeyM fetchEnvVal' conf
  pure $ Docker.toDockerfile . buildEnv <$> env
  where
    fetchEnvVal' k a = lift (fetchEnvVal a) !?? exit (pure $ Left $ FetchEnvErr k)
    buildEnv = Docker.env . Map.toList . Map.mapKeys Text.toUpper

readTemplateDockerFile :: FilePath -> RIO Env (Either BuildError Dockerfile)
readTemplateDockerFile path = do
  logDebug (displayShow $ "read template: " <> path)
  mapLeft ParseErr . Docker.parseText <$> readFileUtf8 path

readBeforeTemplateDockerFile :: RIO Env (Either BuildError Dockerfile)
readBeforeTemplateDockerFile =
  readTemplateDockerFile =<< asks (view #before_env . view #template . view #config)

readAftreTemplateDockerFile :: RIO Env (Either BuildError Dockerfile)
readAftreTemplateDockerFile =
  readTemplateDockerFile =<< asks (view #after_env . view #template . view #config)

mapWithKeyM ::  Monad m => (k -> a -> m b) -> Map k a -> m (Map k b)
mapWithKeyM f = sequence . Map.mapWithKey f

fromText :: IsString s => Text -> s
fromText = fromString . Text.unpack

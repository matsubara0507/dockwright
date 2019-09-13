{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}

module Dockwright.Fetch.GitHub where

import           RIO
import qualified RIO.Text               as Text

import           Data.Extensible
import           Dockwright.Data.Config (GitHubConfig, splitOn)
import           Dockwright.Data.Env
import           Dockwright.Data.GitHub
import           Mix.Plugin.Logger.JSON as MixLogger
import           Network.HTTP.Req

fetchRelease' ::
  (MonadUnliftIO m, MonadReader e m, HasLogFunc e)
  => GitHubConfig -> m (Either FetchError Text)
fetchRelease' conf =
  fmap (stripByConfig conf . view #tag_name) <$> fetchRelease (splitOn '/' $ conf ^. #repo)

fetchRelease ::
  (MonadUnliftIO m, MonadReader e m, HasLogFunc e)
  => (Text, Text) -> m (Either FetchError Release)
fetchRelease (owner, repo) = do
  MixLogger.logDebugR "fetch release from github" (#url @= tshow url <: nil)
  (latest <$> runReq defaultHttpConfig (responseBody <$> request)) `catch` handler
  where
    request = req GET url NoReqBody jsonResponse h
    url = https "api.github.com" /: "repos" /: owner /: repo /: "releases"
    h = header "User-Agent" "Dockwright"
    handler = pure . Left . HttpErr

latest :: [Release] -> Either FetchError Release
latest = maybe (Left NoRelease) pure . listToMaybe

stripByConfig :: GitHubConfig -> Text -> Text
stripByConfig conf release = fromMaybe release $ do
  prefix <- conf ^. #strip_prefix
  Text.stripPrefix prefix release

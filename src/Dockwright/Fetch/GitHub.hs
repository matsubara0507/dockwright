{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}

module Dockwright.Fetch.GitHub where

import           RIO
import qualified RIO.Text               as Text

import           Dockwright.Data.Config (GitHubConfig)
import           Dockwright.Data.Env
import           Dockwright.Data.GitHub
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
  logDebug (display $ "fetch github: " <> tshow url)
  (latest <$> runReq defaultHttpConfig (responseBody <$> request)) `catch` handler
  where
    request = req GET url NoReqBody jsonResponse h
    url = https "api.github.com" /: "repos" /: owner /: repo /: "releases"
    h = header "User-Agent" "Dockwright"
    handler = pure . Left . HttpErr

latest :: [Release] -> Either FetchError Release
latest = maybe (Left NoRelease) pure . listToMaybe

splitOn :: Char -> Text -> (Text, Text)
splitOn c = second (Text.drop 1) . Text.span (/= c)

stripByConfig :: GitHubConfig -> Text -> Text
stripByConfig conf release =
  fromMaybe release $ Text.stripPrefix (conf ^. #strip_prefix) release

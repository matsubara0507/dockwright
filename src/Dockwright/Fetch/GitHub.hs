{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}

module Dockwright.Fetch.GitHub where

import           RIO
import qualified RIO.Text               as Text

import           Data.Default
import           Dockwright.Data.Config (GitHubConfig)
import           Dockwright.Data.Env    (DockwrightException (..))
import           Dockwright.Data.GitHub
import           Network.HTTP.Req

fetchRelease' ::
  (MonadIO m, MonadReader e m, HasLogFunc e) => GitHubConfig -> m (Maybe String)
fetchRelease' conf =
  fetchRelease (splitOn '/' $ conf ^. #repo) >>= \case
    Left err -> do
      logWarn $ displayShow err
      pure Nothing
    Right r  -> pure $ Just (Text.unpack $ r ^. #tag_name)

fetchRelease ::
  MonadIO m => (Text, Text) -> m (Either DockwrightException Release)
fetchRelease (owner, repo) =
  liftIO . fmap latest $
    runReq def (responseBody <$> req GET url NoReqBody jsonResponse h)
  where
    url = https "api.github.com" /: "repos" /: owner /: repo /: "releases"
    h = header "User-Agent" "Dockwright"

latest :: [Release] -> Either DockwrightException Release
latest = maybe (Left $ FetchEnvError "no release") pure . listToMaybe

splitOn :: Char -> Text -> (Text, Text)
splitOn c = second (Text.drop 1) . Text.span (/= c)

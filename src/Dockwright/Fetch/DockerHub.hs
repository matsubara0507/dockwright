module Dockwright.Fetch.DockerHub where

import           RIO

import           Data.Extensible
import           Dockwright.Data.Config    (splitOn)
import           Dockwright.Data.DockerHub
import           Dockwright.Data.Env
import           Mix.Plugin.Logger.JSON    as MixLogger
import           Network.HTTP.Req
import qualified Text.URI                  as URI

fetchImageTags ::
  (MonadUnliftIO m, MonadReader e m, HasLogFunc e)
  => Text -> m (Either FetchError [Tag])
fetchImageTags imageName = do
  MixLogger.logDebugR "fetch tags from DockerHub" (#url @= tshow url <: nil)
  fmap Right (fetchTagsR [] (Just $ "page" =: (1 :: Int))) `catch` handler
  where
    (owner, repo) = splitOn '/' imageName
    url = https "registry.hub.docker.com" /: "v2" /: "repositories" /: owner /: repo /: "tags"
    h = header "User-Agent" "Dockwright"

    buildReq ::  Option 'Https -> Req (JsonResponse Tags)
    buildReq opts = req GET url NoReqBody jsonResponse (h <> opts)

    handler = pure . Left . HttpErr

    fetchTagsR ::
      (MonadIO m, MonadReader e m, HasLogFunc e)
      => [Tag] -> Maybe (Option 'Https) -> m [Tag]
    fetchTagsR xs Nothing = pure xs
    fetchTagsR xs (Just opts) = do
      tags <- runReq defaultHttpConfig (responseBody <$> buildReq opts)
      MixLogger.logDebugR "fetched tags with next url" (#next @= (tags ^. #next) <: nil)
      let nextOpts = fmap snd $ useHttpsURI =<< URI.mkURI =<< tags ^. #next
      threadDelay 100_000
      fetchTagsR (tags ^. #results ++ xs) nextOpts

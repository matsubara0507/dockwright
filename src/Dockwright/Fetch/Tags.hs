module Dockwright.Fetch.Tags where

import           RIO
import qualified RIO.HashMap               as HM
import qualified RIO.Text                  as Text

import qualified Data.Aeson.Types          as J
import           Data.Extensible
import           Data.Fallible
import           Dockwright.Data.Config    (RefTagsConfig)
import           Dockwright.Data.DockerHub (Tag, toTag)
import           Dockwright.Data.Env
import           Mix.Plugin.Logger.JSON    as MixLogger
import           Network.HTTP.Req

fetchTags ::
  (MonadUnliftIO m, MonadReader e m, HasLogFunc e)
  => RefTagsConfig -> m (Either FetchError [Tag])
fetchTags config = do
  MixLogger.logDebugR "fetch tags from tags config" (#url @= (config ^. #ref) <: nil)
  fetchTags' (config ^. #ref) `catch` handler
  where
    h = header "User-Agent" "Dockwright"

    handler = pure . Left . HttpErr

    fetchTags' ref = evalContT $ do
      (url, opts) <- parseUrlHttps (Text.encodeUtf8 ref) ??? exitA (Left $ UrlParseErr ref)
      let request = req GET url NoReqBody jsonResponse (h <> opts)
      resp <- runReq defaultHttpConfig (responseBody <$> request)
      let tags =  flip lookupKey resp . Text.split (== ':') <$> config ^. #keys
      pure (Right $ catMaybes tags)

lookupKey :: [Text] -> J.Value -> Maybe Tag
lookupKey (k:ks) (J.Object hash) = lookupKey ks =<< HM.lookup k hash
lookupKey [] (J.String val)      = Just (toTag val)
lookupKey _ _                    = Nothing

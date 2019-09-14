module Dockwright.Fetch
  ( fetchEnvVal
  , Fetch (..)
  ) where

import           RIO

import           Data.Extensible
import           Dockwright.Data.Config
import           Dockwright.Data.Env
import           Dockwright.Fetch.GitHub
import           Mix.Plugin.Logger       ()

fetchEnvVal :: DockVal -> RIO Env (Either FetchError Text)
fetchEnvVal val = fromMaybe (Left UndefinedConfig) <$>
  hfoldrWithIndexFor
    (Proxy @ Fetch)
    (\m v r -> liftA2 (<|>) (fetch m $ v ^. _Wrapper) r)
    (pure Nothing)
    val

class Fetch kv where
  fetch ::
    (MonadUnliftIO m, MonadReader e m, HasLogFunc e)
    => proxy kv -> TargetOf kv -> m (Maybe (Either FetchError Text))

instance Fetch ("github" >: Maybe GitHubConfig) where
  fetch _ Nothing = pure Nothing
  fetch _ (Just conf) = case conf ^. #hook of
    "release" -> Just <$> fetchRelease' conf
    key       -> pure $ Just (Left $ UndefinedKey key)

instance Fetch ("value" >: Maybe Text) where
  fetch _ = pure . fmap Right

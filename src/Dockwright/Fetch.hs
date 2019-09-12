{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Dockwright.Fetch
  ( fetchEnvVal
  , Fetch (..)
  ) where

import           RIO

import           Data.Extensible
import           Dockwright.Data.Config
import           Dockwright.Data.Env
import           Dockwright.Fetch.GitHub
import           Mix.Plugin.Logger       as MixLogger

fetchEnvVal :: DockVal -> RIO Env (Maybe Text)
fetchEnvVal val = hfoldrWithIndexFor
  (Proxy @ Fetch)
  (\m v r -> liftA2 (<|>) (fetch' m $ v ^. _Wrapper) r)
  (pure Nothing)
  val
  where
    fetch' m v  = either handler (pure . Just) =<< fetch m v
    handler err = MixLogger.logWarn (displayFetchError err) >> pure Nothing

class Fetch kv where
  fetch ::
    (MonadUnliftIO m, MonadReader e m, HasLogFunc e)
    => proxy kv -> TargetOf kv -> m (Either FetchError Text)

instance Fetch ("github" >: Maybe GitHubConfig) where
  fetch _ Nothing = pure (Left UndefinedConfig)
  fetch _ (Just conf) = case conf ^. #hook of
    "release" -> fetchRelease' conf
    key       -> pure (Left $ UndefinedKey key)

instance Fetch ("value" >: Maybe Text) where
  fetch _ = pure . maybe (Left UndefinedConfig) Right

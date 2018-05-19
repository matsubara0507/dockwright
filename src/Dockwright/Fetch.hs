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
import qualified RIO.Text                as Text

import           Data.Extensible
import           Data.Proxy
import           Dockwright.Data.Config
import           Dockwright.Data.Env
import           Dockwright.Fetch.GitHub

fetchEnvVal :: DockVal -> RIO Env String
fetchEnvVal val =
  maybe (throwM err) pure =<<
    hfoldrWithIndexFor
      (Proxy @ Fetch)
      (\m v r -> liftA2 (<|>) (fetch m $ v ^. _Wrapper) r)
      (pure Nothing)
      val
  where
    err = FetchEnvError "there is no config to fetch env."

class Fetch kv where
  fetch ::
    (MonadIO m, MonadReader env m, HasLogFunc env)
    => proxy kv -> AssocValue kv -> m (Maybe String)

instance Fetch ("github" >: Maybe GitHubConfig) where
  fetch _ = \case
    Nothing   -> pure Nothing
    Just conf -> case conf ^. #hook of
      "release" -> fetchRelease' conf
      _         -> pure Nothing

instance Fetch ("value" >: Maybe Text) where
  fetch _ = pure . fmap Text.unpack

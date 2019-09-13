{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}

module Dockwright.Tags where

import           RIO

import           Dockwright.Data.Config
import           Dockwright.Data.DockerHub (Tag, toTag)
import           Dockwright.Data.Env
import           Dockwright.Fetch.Tags     (fetchTags)

collectTags ::
  (MonadUnliftIO m, MonadReader e m, HasLogFunc e)
  => TagsConfig -> m (Either TagsError [Tag])
collectTags conf = case conf ^. #type of
  "value" -> pure (Right $ toTag <$> toValTagsConfig conf ^. #keys)
  "refer" -> mapLeft FetchErr <$> fetchTags (toRefTagsConfig conf)
  typ     -> pure (Left $ UndefinedType typ)

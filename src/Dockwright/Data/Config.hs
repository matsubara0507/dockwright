module Dockwright.Data.Config
  ( module X
  , readConfig
  , toValTagsConfig
  , toRefTagsConfig
  , splitOn
  ) where

import           RIO
import qualified RIO.Text                        as Text

import           Data.Extensible
import qualified Data.Yaml                       as Y
import           Dockwright.Data.Config.Default  as X
import           Dockwright.Data.Config.Internal as X

readConfig :: MonadIO m => FilePath -> m (Either Y.ParseException Config)
readConfig = readConfigWith defaultConfig

readConfigWith ::
  MonadIO m => Config -> FilePath -> m (Either Y.ParseException Config)
readConfigWith def path = do
  file <- readFileBinary path
  pure $ case Y.decodeEither' file of
    Right Y.Null -> Right def
    _            -> hzipWith fromNullable def <$> Y.decodeEither' file

toValTagsConfig :: TagsConfig -> ValTagsConfig
toValTagsConfig = shrink

toRefTagsConfig :: TagsConfig -> RefTagsConfig
toRefTagsConfig conf
    = #ref @= fromMaybe "" (conf ^. #ref)
   <: #keys @= (conf ^. #keys)
   <: nil

splitOn :: Char -> Text -> (Text, Text)
splitOn c = second (Text.drop 1) . Text.span (/= c)

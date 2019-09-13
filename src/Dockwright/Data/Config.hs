module Dockwright.Data.Config where

import           RIO
import qualified RIO.Text        as Text

import           Data.Extensible

type Config = Record
   '[ "image"    >: Text
    , "output"   >: FilePath
    , "template" >: DockerfileTeamplate
    , "base"     >: BaseImageConfig
    , "env"      >: Map Text DockVal
    , "tags"     >: [TagsConfig]
    ]

type DockerfileTeamplate = Record
   '[ "before_env" >: Maybe FilePath
    , "after_env"  >: Maybe FilePath
    ]

type BaseImageConfig = Record
   '[ "repo" >: Text
    , "tag"  >: Text
    ]

type DockVal = Record
   '[ "github" >: Maybe GitHubConfig
    , "value"  >: Maybe Text
    ]

type GitHubConfig = Record
   '[ "repo"         >: Text
    , "hook"         >: Text
    , "strip_prefix" >: Maybe Text
    ]

type TagsConfig = Record
   '[ "type"   >: Text
    , "ref"    >: Maybe Text
    , "keys"   >: [Text]
    , "always" >: Maybe Bool
    ]

type ValTagsConfig = Record
   '[ "keys"   >: [Text]
    ]

toValTagsConfig :: TagsConfig -> ValTagsConfig
toValTagsConfig = shrink

type RefTagsConfig = Record
   '[ "ref"    >: Text
    , "keys"   >: [Text]
    ]

toRefTagsConfig :: TagsConfig -> RefTagsConfig
toRefTagsConfig conf
    = #ref @= fromMaybe "" (conf ^. #ref)
   <: #keys @= (conf ^. #keys)
   <: nil

splitOn :: Char -> Text -> (Text, Text)
splitOn c = second (Text.drop 1) . Text.span (/= c)

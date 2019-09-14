module Dockwright.Data.Config.Internal where

import           RIO

import           Data.Extensible

type Config = Record ConfigFields

type ConfigFields =
   '[ "image"    >: Text
    , "output"   >: FilePath
    , "template" >: DockerfileTeamplate
    , "base"     >: BaseImageConfig
    , "env"      >: Maybe (Map Text DockVal)
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

type RefTagsConfig = Record
   '[ "ref"    >: Text
    , "keys"   >: [Text]
    ]

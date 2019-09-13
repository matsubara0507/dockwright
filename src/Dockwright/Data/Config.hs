{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

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

splitOn :: Char -> Text -> (Text, Text)
splitOn c = second (Text.drop 1) . Text.span (/= c)

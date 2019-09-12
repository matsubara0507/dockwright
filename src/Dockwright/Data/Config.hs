{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Dockwright.Data.Config where

import           RIO

import           Data.Extensible

type Config = Record
   '[ "output"   >: FilePath
    , "template" >: FilePath
    , "base"     >: BaseImageConfig
    , "env"      >: Map Text DockVal
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
    , "strip_prefix" >: Text
    ]

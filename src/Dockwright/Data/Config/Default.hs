{-# LANGUAGE TemplateHaskell #-}

module Dockwright.Data.Config.Default
  ( defaultConfig
  , defaultConfig'
  ) where

import           RIO

import           Data.FileEmbed                  (embedFile)
import           Data.Yaml.TH                    (decodeFile)
import           Dockwright.Data.Config.Internal as X
import           Instances.TH.Lift               ()

defaultConfig :: Config
defaultConfig = $$(decodeFile "./template/.dockwright.yaml")

defaultConfig' :: ByteString
defaultConfig' = $(embedFile "./template/.dockwright.yaml")

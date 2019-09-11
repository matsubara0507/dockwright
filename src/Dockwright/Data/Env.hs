{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dockwright.Data.Env
    ( Env
    , DockwrightException (..)
    ) where

import           RIO
import qualified RIO.Text               as Text

import           Data.Extensible
import           Dockwright.Data.Config (Config)
import qualified Language.Docker        as Docker
import qualified Language.Docker.Parser as Docker

type Env = Record
  '[ "config" >: Config
   , "logger" >: LogFunc
   ]

instance Lookup xs "logger" LogFunc => HasLogFunc (Record xs) where
  logFuncL = lens (view #logger) (\x y -> x & #logger `set` y)

data DockwrightException
    = DockerfileParseError Docker.Error
    | FetchEnvError Text
    | EchoEnvError Text
    deriving (Typeable)

instance Exception DockwrightException

instance Show DockwrightException where
  show = \case
    DockerfileParseError err -> "dockerfile parse error: " <> Docker.errorBundlePretty err
    FetchEnvError err        -> "fetch env error: " <> Text.unpack err
    EchoEnvError key         -> "echo env error: not found " <> Text.unpack key

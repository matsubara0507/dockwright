{-# LANGUAGE DataKinds        #-}
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

type Env = Record
  '[ "config" >: Config
   , "logger" >: LogFunc
   ]

instance HasLogFunc Env where
  logFuncL = lens (view #logger) (\x y -> x & #logger `set` y)

data DockwrightException
    = DockerfileParseError Docker.ParseError
    | FetchEnvError Text
    deriving (Typeable)

instance Exception DockwrightException

instance Show DockwrightException where
  show = \case
    DockerfileParseError err -> "dockerfile parse error: " <> show err
    FetchEnvError err        -> "fetch env error: " <> Text.unpack err

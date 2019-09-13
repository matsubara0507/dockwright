{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dockwright.Data.Env
    ( Env
    , FetchError (..)
    , displayFetchError
    , BuildError (..)
    , displayBuildError
    , TagsError (..)
    , displayTagsError
    ) where

import           RIO
import qualified RIO.Text               as Text

import           Data.Extensible
import           Dockwright.Data.Config (Config)
import qualified Language.Docker        as Docker
import qualified Language.Docker.Parser as Docker
import           Network.HTTP.Req       (HttpException)

type Env = Record
  '[ "config" >: Config
   , "logger" >: LogFunc
   ]

data FetchError
   = HttpErr HttpException
   | NoRelease
   | UndefinedKey Text
   | UndefinedConfig
   | UrlParseErr Text

displayFetchError :: IsString s => FetchError -> s
displayFetchError = \case
  HttpErr err      -> fromString $ "HTTP Error: " <> show err
  NoRelease        -> "no release"
  UndefinedKey key -> fromString $ "unknown GitHub hook key: " <> Text.unpack key
  UndefinedConfig  -> "undefine cofig"
  UrlParseErr url  -> fromString $ "can not parse url: " <> Text.unpack url

data BuildError
  = FetchEnvErr Text
  | ParseErr Docker.Error

displayBuildError :: IsString s => BuildError -> s
displayBuildError = \case
  FetchEnvErr key -> fromString $ "fetch env error: " <> Text.unpack key
  ParseErr err    -> fromString $ Docker.errorBundlePretty err

data TagsError
  = FetchErr FetchError
  | UndefinedType Text

displayTagsError :: IsString s => TagsError -> s
displayTagsError = \case
  FetchErr err      -> displayFetchError err
  UndefinedType typ -> fromString $ "undefined tags config type: " <> Text.unpack typ

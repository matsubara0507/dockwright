{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Dockwright.Data.GitHub where

import           RIO

import           Data.Extensible

type Release = Record
    '[ "url"        >: Text
     , "html_url"   >: Text
     , "id"         >: Int
     , "tag_name"   >: Text
     , "name"       >: Maybe Text
     , "draft"      >: Bool
     , "prerelease" >: Bool
     ]

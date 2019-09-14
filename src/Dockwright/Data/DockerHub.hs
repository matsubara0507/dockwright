module Dockwright.Data.DockerHub where

import           RIO

import           Data.Extensible

type Tags = Record
    '[ "count"    >: Int
     , "next"     >: Maybe Text
     , "results"  >: [Tag]
     ]

type Tag = Record
    '[ "name"         >: Text
     , "last_updated" >: Maybe Text
     ]

toTag :: Text -> Tag
toTag name
    = #name @= name
   <: #last_updated @= Nothing
   <: nil

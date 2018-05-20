{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeOperators    #-}

module Main where

import           Paths_dockwright       (version)
import           RIO

import           Data.Extensible
import           Data.Extensible.GetOpt
import           Data.Version           (Version)
import qualified Data.Version           as Version
import           Data.Yaml
import           Development.GitRev
import           Dockwright.Build
import qualified Language.Docker        as Docker

main :: IO ()
main = withGetOpt "[options] [config-file]" info $ \opts args -> if
  | opts ^. #version -> hPutBuilder stdout (buildVersion version)
  | otherwise -> buildDockerFile opts args
  where
    info :: RecordOf (OptionDescr Identity) Options
    info
        = #verbose @= optFlag "v" ["verbose"] "Enable verbose mode"
       <: #version @= optFlag [] ["version"] "Show version"
       <: nil

buildDockerFile :: Record Options -> [String] -> IO ()
buildDockerFile opts args = do
  let path = fromMaybe "./.dockwright.yaml" $ listToMaybe args
  logOpts <- logOptionsHandle stdout (opts ^. #verbose)
  fmap decodeEither (readFileBinary path) >>= \case
    Left  err    -> error $ "yaml parse error: " <> err
    Right config -> do
      file <- withLogFunc logOpts $ \logger -> do
        let env = #config @= config
               <: #logger @= logger
               <: nil
        runRIO env build
      writeFileUtf8 "Dockerfile" (fromString $ Docker.prettyPrint file)

type Options =
   '[ "verbose" >: Bool
    , "version" >: Bool
    ]

buildVersion :: Version -> Builder
buildVersion v = encodeUtf8Builder . fromString $ unwords
  [ "Version"
  , Version.showVersion v ++ ","
  , "Git revision"
  , $(gitHash)
  , "(" ++ $(gitCommitCount) ++ " commits)"
  ]

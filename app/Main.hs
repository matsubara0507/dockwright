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

type Options =
   '[ "verbose" >: Bool
    , "version" >: Bool
    ]

buildDockerFile :: Record Options -> [String] -> IO ()
buildDockerFile opts args = do
  logOpts <- logOptionsHandle stdout (opts ^. #verbose)
  withLogFunc logOpts $ \logger -> do
    let path = fromMaybe "./.dockwright.yaml" $ listToMaybe args
    runRIO (#logger @== logger <: nil) $
      logDebug (displayShow $ "read config yaml: " <> path)
    fmap decodeEither (readFileBinary path) >>= \case
      Left  err    -> error $ "yaml parse error: " <> err
      Right config -> do
        let env = #config @= config
               <: #logger @= logger
               <: nil
        runRIO env $ do
          logDebug "build Dockerfile from template and config yaml."
          file <- build
          let opath = config ^. #output <> "/Dockerfile"
          logDebug (displayShow $ "write Dockerfile: " <> opath)
          writeFileUtf8 opath (fromString $ Docker.prettyPrint file)
          logInfo "Build Sccuess!"

buildVersion :: Version -> Builder
buildVersion v = encodeUtf8Builder . fromString $ unwords
  [ "Version"
  , Version.showVersion v ++ ","
  , "Git revision"
  , $(gitHash)
  , "(" ++ $(gitCommitCount) ++ " commits)"
  ]

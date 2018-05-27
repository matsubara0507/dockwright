{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeOperators    #-}

module Main where

import           Paths_dockwright       (version)
import           RIO
import qualified RIO.Text               as Text

import           Data.Extensible
import           Data.Extensible.GetOpt
import           Data.Version           (Version)
import qualified Data.Version           as Version
import           Data.Yaml
import           Development.GitRev
import           Dockwright.Build
import           Dockwright.Data.Env
import qualified Language.Docker        as Docker

main :: IO ()
main = withGetOpt "[options] [config-file]" info $ \opts args -> if
  | opts ^. #version       -> hPutBuilder stdout (buildVersion version)
  | isJust (opts ^. #echo) -> hPutBuilder stdout =<< getEnvInDockerfile opts args
  | otherwise              -> buildDockerFile opts args
  where
    info :: RecordOf (OptionDescr Identity) Options
    info
        = #verbose @= optFlag "v" ["verbose"] "Enable verbose mode"
       <: #version @= optFlag [] ["version"] "Show version"
       <: #echo    @= optionOptArg (pure . listToMaybe . catMaybes) [] ["echo"] "ENV" "Show fetched env after build"
       <: nil

type Options =
   '[ "verbose" >: Bool
    , "version" >: Bool
    , "echo"    >: Maybe String
    ]

buildDockerFile :: Record Options -> [String] -> IO ()
buildDockerFile = runApp $ \_opts _args -> do
  config <- asks (view #config)
  logDebug "build Dockerfile from template and config yaml."
  file <- build
  let opath = config ^. #output <> "/Dockerfile"
  logDebug (displayShow $ "write Dockerfile: " <> opath)
  writeFileUtf8 opath (fromString $ Docker.prettyPrint file)
  logInfo "Build Sccuess!"

buildVersion :: Version -> Builder
buildVersion v = toBuilder $ unwords
  [ "Version"
  , Version.showVersion v ++ ","
  , "Git revision"
  , $(gitHash)
  , "(" ++ $(gitCommitCount) ++ " commits)"
  ]

getEnvInDockerfile :: Record Options -> [String] -> IO Builder
getEnvInDockerfile = runApp $ \opts _args -> do
  config <- asks (view #config)
  let opath = config ^. #output <> "/Dockerfile"
      key   = fromMaybe "" (opts ^. #echo)
  logDebug (displayShow $ "read Dockerfile: " <> opath)
  (Docker.parseString . Text.unpack <$> readFileUtf8 opath) >>= \case
    Left err   -> throwM $ DockerfileParseError err
    Right file -> do
      let vals = map (lookupEnv key . Docker.instruction) file
      maybe (throwM $ EchoEnvError key) (pure . toBuilder) $ listToMaybe (catMaybes vals)
  where
    lookupEnv key (Docker.Env env) = lookup key env
    lookupEnv _ _                  = Nothing

runApp ::
  (Record Options -> [String] -> RIO Env a) -> Record Options -> [String] -> IO a
runApp prog opts args = do
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
        runRIO env (prog opts args `catch` handler)
  where
    handler :: DockwrightException -> m a
    handler = error . show

toBuilder :: String -> Builder
toBuilder = encodeUtf8Builder . fromString

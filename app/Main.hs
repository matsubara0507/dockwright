module Main where

import           Paths_dockwright       (version)
import           RIO
import qualified RIO.ByteString         as B
import           RIO.List               ((\\))
import qualified RIO.List               as L
import qualified RIO.Text               as Text

import           Data.Extensible
import           Data.Extensible.GetOpt
import           Data.Fallible
import qualified Data.Yaml              as Y
import qualified Dockwright
import           GetOpt                 (withGetOpt')
import qualified Language.Docker        as Docker
import           Mix
import           Mix.Plugin.Logger      as MixLogger
import           Mix.Plugin.Logger.JSON as MixLogger
import qualified Version

main :: IO ()
main = withGetOpt' "[options] [config-file]" info $ \opts args usage -> if
  | opts ^. #help          -> hPutBuilder stdout (fromString usage)
  | opts ^. #version       -> hPutBuilder stdout (Version.build version)
  | opts ^. #default       -> B.putStr (Dockwright.defaultConfig' <> "\n")
  | isJust (opts ^. #echo) -> getEnvInDockerfile opts args
  | opts ^. #tags          -> fetchTagsByDockerHub opts args
  | opts ^. #new_tags      -> fetchNewTags opts args
  | otherwise              -> buildDockerFile opts args
  where
    info
        = #help      @= optFlag "h" ["help"] "Show this help text"
       <: #verbose   @= optFlag "v" ["verbose"] "Enable verbose mode"
       <: #version   @= optFlag [] ["version"] "Show version"
       <: #default   @= optFlag "d" ["default"] "Dump default config"
       <: #echo      @= optionOptArg (pure . firstJust) [] ["echo"] "ENV" "Show fetched env after build"
       <: #tags      @= optFlag [] ["tags"] "Fetch docker image tags from DockerHub"
       <: #new_tags  @= optFlag [] ["new-tags"] "Fetch new tags from tags config"
       <: #with_name @= optFlag [] ["with-name"] "Append image name to display tag"
       <: nil

type Options =
   '[ "help"      >: Bool
    , "verbose"   >: Bool
    , "version"   >: Bool
    , "default"   >: Bool
    , "echo"      >: Maybe String
    , "tags"      >: Bool
    , "new_tags"  >: Bool
    , "with_name" >: Bool
    ]

buildDockerFile :: Record Options -> [String] -> IO ()
buildDockerFile = runApp $ \_opts -> evalContT $ do
  config <- asks (view #config)
  MixLogger.logDebugR "build Dockerfile from template and config yaml" nil
  file <- lift Dockwright.build !?= exit . buildError
  let opath = config ^. #output <> "/Dockerfile"
  MixLogger.logDebugR "write Dockerfile" (#path @= opath <: nil)
  liftIO $ Docker.writeDockerFile (fromString opath) file
  MixLogger.logInfo "Build Sccuess!"
  where
    buildError err = MixLogger.logError (Dockwright.displayBuildError err)

getEnvInDockerfile :: Record Options -> [String] -> IO ()
getEnvInDockerfile = runApp $ \opts -> evalContT $ do
    config <- asks (view #config)
    let opath = config ^. #output <> "/Dockerfile"
        key   = maybe "" fromString (opts ^. #echo)
    MixLogger.logDebugR "read Dockerfile" (#path @= opath <: nil)
    file <- lift (Docker.parseText <$> readFileUtf8 opath) !?= exit . decodeError
    let vals = map (lookupEnv (Text.toUpper key) . Docker.instruction) file
    env <- firstJust vals ??? exit (lookupError key)
    MixLogger.logInfo $ display env
  where
    lookupEnv key (Docker.Env env) = lookup key env
    lookupEnv _ _                  = Nothing

    decodeError err =
      MixLogger.logError (fromString $ Docker.errorBundlePretty err)

    lookupError key =
      MixLogger.logError (fromString $ "echo env error: not found " <> Text.unpack key)

fetchTagsByDockerHub :: Record Options -> [String] -> IO ()
fetchTagsByDockerHub = runApp $ \opts -> evalContT $ do
  imageName <- asks (view #image . view #config)
  tags <- lift (Dockwright.fetchImageTags imageName) !?= exit . fetchError

  let prefix = if opts ^. #with_name then imageName <> ":" else ""
  forM_ (reverse $ L.sortOn (view #last_updated) tags) $ \tag ->
    MixLogger.logInfo $ display (prefix <> tag ^. #name)
  where
    fetchError err =
      MixLogger.logError (fromString $ Dockwright.displayFetchError err)

fetchNewTags :: Record Options -> [String] -> IO ()
fetchNewTags = runApp $ \opts -> evalContT $ do
  config <- asks (view #config)
  currentTags <- lift (Dockwright.fetchImageTags $ config ^. #image) !?= exit . fetchError

  tags <- forM (config ^. #tags) $ \tagConf ->
    fmap (fromMaybe False $ tagConf ^. #always,) $
      lift (Dockwright.collectTags tagConf) !?= (\e -> tagsError e >> pure [])

  let (forceTagNames, tagNames) = L.nub <$> mapBoth (L.partition fst tags)
      currentTagsNames = view #name <$> currentTags
      newTagNames = L.nub $ L.sort $ forceTagNames ++ (tagNames \\ currentTagsNames)
  MixLogger.logDebugR "collected tag names"
      $ #current   @= currentTagsNames
     <: #updatable @= tagNames
     <: #force     @= forceTagNames
     <: nil

  let prefix = if opts ^. #with_name then config ^. #image <> ":" else ""
  forM_ newTagNames $ \tagName ->
    MixLogger.logInfo $ display (prefix <> tagName)
  where
    fetchError err =
      MixLogger.logError (fromString $ Dockwright.displayFetchError err)
    tagsError err =
      MixLogger.logError (fromString $ Dockwright.displayTagsError err)

    mapBoth = join (***) (map (view #name) . concatMap snd)

runApp ::
  (Record Options -> RIO Dockwright.Env ())
  -> Record Options
  -> [String]
  -> IO ()
runApp app opts args = Mix.run logging app'
  where
    app' :: RIO (Record '["logger" >: LogFunc]) ()
    app' = evalContT $ do
      let path = fromMaybe "./.dockwright.yaml" $ listToMaybe args
      MixLogger.logDebugR "read config yaml" (#path @= path <: nil)
      config  <- Dockwright.readConfig path !?= exit . decodeError
      logger' <- asks (view #logger)
      let plugin = hsequence
              $ #config <@=> pure config
             <: #logger <@=> pure logger'
             <: nil
      lift $ Mix.run plugin (app opts)

    decodeError err =
      MixLogger.logError (fromString $ Y.prettyPrintParseException err)

    logOpt
        = #handle @= stdout
       <: #verbose @= opts ^. #verbose
       <: nil

    logging = hsequence
        $ #logger <@=> MixLogger.buildPlugin logOpt
       <: nil

firstJust :: [Maybe a] -> Maybe a
firstJust = listToMaybe . catMaybes

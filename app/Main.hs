{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}

module Main where

import           RIO

import           Data.Extensible
import           Data.Yaml
import           Dockwright.Build
import qualified Language.Docker    as Docker
import           System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let path = fromMaybe "./.dockwright.yaml" $ listToMaybe args
  fmap decodeEither (readFileBinary path) >>= \case
    Left  err    -> error $ "yaml parse error: " <> err
    Right config -> do
      logOpts <- logOptionsHandle stdout False
      file <- withLogFunc logOpts $ \logger -> do
        let env = #config @= config
               <: #logger @= logger
               <: nil
        runRIO env build
      writeFileUtf8 "Dockerfile" (fromString $ Docker.prettyPrint file)

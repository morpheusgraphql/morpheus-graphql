{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main,
  )
where

import CLI.Commands
import CLI.Config
import CLI.File (processFileName, saveDocument)
import CLI.Generator
import qualified Data.ByteString.Lazy as L
  ( readFile,
  )
import Data.Version (showVersion)
import qualified Paths_morpheus_graphql_code_gen as CLI
import Relude hiding (ByteString)

currentVersion :: String
currentVersion = showVersion CLI.version

main :: IO ()
main = defaultParser >>= runApp

runApp :: App -> IO ()
runApp App {..}
  | version options = putStrLn currentVersion
  | otherwise = runOperation operations
  where
    runOperation About = putStrLn $ "Morpheus GraphQL CLI, version " <> currentVersion
    runOperation Build {source} = traverse_ (build options) source
    runOperation (Scan path) = scan options path

build :: Options -> FilePath -> IO ()
build options path = do
  file <- L.readFile path
  saveDocument hsPath (processDocument options hsPath file)
  where
    hsPath = processFileName path

scan :: Options -> FilePath -> IO ()
scan options path = do
  config <- readConfig path
  print config
  file <- L.readFile path
  saveDocument hsPath (processDocument options hsPath file)
  where
    hsPath = processFileName path
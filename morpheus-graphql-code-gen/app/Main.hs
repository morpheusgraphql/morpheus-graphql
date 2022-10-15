{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main,
  )
where

import CLI.Commands
  ( App (..),
    BuildOptions (..),
    GlobalOptions (..),
    Operation (..),
    parseCLI,
  )
import CLI.Config
import CLI.File (processFileName, saveDocument)
import CLI.Generator
import qualified Data.ByteString.Lazy as L
  ( readFile,
  )
import Data.Text (unpack)
import Data.Version (showVersion)
import qualified Paths_morpheus_graphql_code_gen as CLI
import Relude hiding (ByteString)
import System.FilePath (normalise, (</>))
import System.FilePath.Glob (glob)

currentVersion :: String
currentVersion = showVersion CLI.version

main :: IO ()
main = parseCLI >>= runApp

runApp :: App -> IO ()
runApp App {..}
  | version options = putStrLn currentVersion
  | otherwise = runOperation operations
  where
    runOperation About = putStrLn $ "Morpheus GraphQL CLI, version " <> currentVersion
    runOperation Build {source} = traverse_ scan source

scan :: FilePath -> IO ()
scan path = do
  Config {server, client} <- readConfig path
  traverse_ (handleServerService path) (concat $ maybeToList server)
  traverse_ (handleClientService path) (concat $ maybeToList client)

handleClientService :: FilePath -> Service -> IO ()
handleClientService target Service {name, source, includes, options} = do
  let root = normalise (target </> unpack source)
  let namespaces = maybe False namespace options
  let patterns = map (normalise . (root </>) . unpack) includes
  files <- concat <$> traverse glob patterns
  putStrLn ("\n build:" <> unpack name)
  traverse_ (buildFile (BuildOptions {..}) {root, namespaces}) files

handleServerService :: FilePath -> Service -> IO ()
handleServerService target Service {name, source, includes, options} = do
  let root = normalise (target </> unpack source)
  let namespaces = maybe False namespace options
  let patterns = map (normalise . (root </>) . unpack) includes
  files <- concat <$> traverse glob patterns
  putStrLn ("\n build:" <> unpack name)
  traverse_ (buildFile (BuildOptions {..}) {root, namespaces}) files

buildFile :: BuildOptions -> FilePath -> IO ()
buildFile options path = do
  putStr ("  - " <> path <> "\n")
  file <- L.readFile path
  saveDocument hsPath (processDocument options hsPath file)
  where
    hsPath = processFileName path

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
  ( Config (..),
    Service (..),
    ServiceOptions (..),
    readConfig,
  )
import CLI.File (processFileName, saveDocument)
import CLI.Generator
import qualified Data.ByteString.Lazy as L
  ( readFile,
  )
import Data.Morpheus.Client (readSchemaSource)
import qualified Data.Text.IO as TIO
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
handleClientService target Service {name, source, includes, options, schema} = do
  let root = normalise (target </> source)
  let namespaces = maybe False namespace options
  let patterns = map (normalise . (root </>)) includes
  files <- concat <$> traverse glob patterns
  putStrLn ("\n build:" <> name)
  schemaPath <- maybe (fail $ "client service " <> name <> " should provide schema!") pure schema
  traverse_ (buildClient (BuildOptions {..}) {root, namespaces} (normalise $ root </> schemaPath)) files

buildClient :: BuildOptions -> FilePath -> FilePath -> IO ()
buildClient options schemaPath queryPath = do
  putStr ("  - " <> queryPath <> "\n")
  file <- TIO.readFile queryPath
  schemaDoc <- readSchemaSource schemaPath
  saveDocument hsPath (processClientDocument options schemaDoc (Just file))
  where
    hsPath = processFileName queryPath

handleServerService :: FilePath -> Service -> IO ()
handleServerService target Service {name, source, includes, options} = do
  let root = normalise (target </> source)
  let namespaces = maybe False namespace options
  let patterns = map (normalise . (root </>)) includes
  files <- concat <$> traverse glob patterns
  putStrLn ("\n build:" <> name)
  traverse_ (buildServer (BuildOptions {..}) {root, namespaces}) files

buildServer :: BuildOptions -> FilePath -> IO ()
buildServer options path = do
  putStr ("  - " <> path <> "\n")
  file <- L.readFile path
  saveDocument hsPath (processServerDocument options hsPath file)
  where
    hsPath = processFileName path

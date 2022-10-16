{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main,
  )
where

import CLI.Commands
  ( App (..),
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
import CLI.File (getModuleNameByPath, processFileName, saveDocument)
import CLI.Generator
import qualified Data.ByteString.Lazy as L
  ( readFile,
  )
import Data.Morpheus.Client (readSchemaSource)
import Data.Text (pack)
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

getImports :: Maybe ServiceOptions -> [Text]
getImports = concat . maybeToList . (>>= globals)

handleClientService :: FilePath -> Service -> IO ()
handleClientService target Service {name, source, includes, options, schema} = do
  let root = normalise (target </> source)
  let namespaces = fromMaybe False (options >>= namespace)
  let patterns = map (normalise . (root </>)) includes
  files <- concat <$> traverse glob patterns
  putStrLn ("\n build:" <> name)
  schemaPath <- maybe (fail $ "client service " <> name <> " should provide schema!") pure schema
  let globalImports = getImports options
  let config = BuildConfig {..}
  buildClientGlobals config (normalise $ root </> schemaPath)
  traverse_ (buildClientQuery config (normalise $ root </> schemaPath)) files

buildClientGlobals :: BuildConfig -> FilePath -> IO ()
buildClientGlobals options schemaPath = do
  putStr ("  - " <> schemaPath <> "\n")
  schemaDoc <- readSchemaSource schemaPath
  let hsPath = processFileName schemaPath
  let moduleName = pack $ getModuleNameByPath (root options) hsPath
  saveDocument hsPath (processClientDocument options schemaDoc Nothing moduleName)

buildClientQuery :: BuildConfig -> FilePath -> FilePath -> IO ()
buildClientQuery options schemaPath queryPath = do
  putStr ("  - " <> queryPath <> "\n")
  file <- TIO.readFile queryPath
  schemaDoc <- readSchemaSource schemaPath
  let moduleName = getModuleNameByPath (root options) hsPath
  let globalModuleName = pack (getModuleNameByPath (root options) (processFileName schemaPath))
  saveDocument hsPath (processClientDocument (options {globalImports = [globalModuleName]}) schemaDoc (Just file) (pack moduleName))
  where
    hsPath = processFileName queryPath

handleServerService :: FilePath -> Service -> IO ()
handleServerService target Service {name, source, includes, options} = do
  let root = normalise (target </> source)
  let namespaces = fromMaybe False (options >>= namespace)
  let patterns = map (normalise . (root </>)) includes
  files <- concat <$> traverse glob patterns
  putStrLn ("\n build:" <> name)
  let globalImports = getImports options
  traverse_ (buildServer (BuildConfig {..}) {root, namespaces}) files

buildServer :: BuildConfig -> FilePath -> IO ()
buildServer options path = do
  putStr ("  - " <> path <> "\n")
  file <- L.readFile path
  let moduleName = getModuleNameByPath (root options) hsPath
  saveDocument hsPath (processServerDocument options moduleName file)
  where
    hsPath = processFileName path

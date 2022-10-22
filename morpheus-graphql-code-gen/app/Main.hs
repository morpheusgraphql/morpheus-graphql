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
    Command (..),
    GlobalOptions (..),
    parseCLI,
  )
import CLI.Config
  ( Config (..),
    Service (..),
    ServiceOptions (..),
    readConfig,
  )
import CLI.File (getModuleNameByPath, processDocument, processFileName)
import CLI.Generator
  ( BuildConfig (..),
    processClientDocument,
    processServerDocument,
  )
import qualified Data.ByteString.Lazy as L
  ( readFile,
  )
import Data.Morpheus.Client (readSchemaSource)
import Data.Text (pack)
import qualified Data.Text.IO as TIO
import Data.Version (showVersion)
import qualified Paths_morpheus_graphql_code_gen as CLI
import Relude hiding (ByteString)
import System.Exit (ExitCode (..))
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
    runOperation (Build source) = processAll (scan . Context False) source
    runOperation (Check source) = processAll (scan . Context True) source

data Context = Context
  { isCheck :: Bool,
    configDir :: FilePath
  }

type CommandResult = Bool

processAll :: (Traversable t, MonadIO m) => (a1 -> m Bool) -> t a1 -> m b
processAll f xs = do
  res <- traverse f xs
  if and res
    then putStr "\x1b[32mOK\x1b[0m\n" >> exitSuccess
    else exitWith (ExitFailure 1)

scan :: Context -> IO CommandResult
scan ctx = do
  Config {server, client} <- readConfig (configDir ctx)
  servers <- traverse (handleServerService ctx) (concat $ maybeToList server)
  clients <- traverse (handleClientService ctx) (concat $ maybeToList client)
  pure $ and (servers <> clients)

getImports :: Maybe ServiceOptions -> [Text]
getImports = concat . maybeToList . (>>= globals)

parseServiceData :: Context -> Service -> IO (FilePath, Bool, [FilePath], [Text])
parseServiceData ctx Service {source, includes, options} = do
  let root = normalise (configDir ctx </> source)
  let namespaces = fromMaybe False (options >>= namespace)
  let patterns = map (normalise . (root </>)) includes
  files <- concat <$> traverse glob patterns
  let globalImports = getImports options
  pure (root, namespaces, files, globalImports)

getSchemaPath :: MonadFail m => FilePath -> String -> Maybe FilePath -> m FilePath
getSchemaPath root name schema = do
  schemaPath <- maybe (fail $ "client service " <> name <> " should provide schema!") pure schema
  pure $ normalise $ root </> schemaPath

handleClientService :: Context -> Service -> IO CommandResult
handleClientService ctx s@Service {name, schema} = do
  (root, namespaces, files, globalImports) <- parseServiceData ctx s
  putStrLn ("\n build:" <> name)
  schemaPath <- getSchemaPath root name schema
  let config = BuildConfig {..}
  globals <- buildClientGlobals ctx config schemaPath
  and . (globals :) <$> traverse (buildClientQuery ctx config schemaPath) files

buildClientGlobals :: Context -> BuildConfig -> FilePath -> IO CommandResult
buildClientGlobals ctx options schemaPath = do
  putStr ("  - " <> schemaPath <> "\n")
  schemaDoc <- readSchemaSource schemaPath
  let hsPath = processFileName schemaPath
  let moduleName = pack $ getModuleNameByPath (root options) hsPath
  let result = processClientDocument options schemaDoc Nothing moduleName
  processDocument (isCheck ctx) hsPath result

getSchemaImports :: BuildConfig -> FilePath -> [Text]
getSchemaImports options schemaPath = [pack (getModuleNameByPath (root options) (processFileName schemaPath))]

buildClientQuery :: Context -> BuildConfig -> FilePath -> FilePath -> IO CommandResult
buildClientQuery ctx options schemaPath queryPath = do
  putStr ("  - " <> queryPath <> "\n")
  file <- TIO.readFile queryPath
  schemaDoc <- readSchemaSource schemaPath
  let moduleName = getModuleNameByPath (root options) hsPath
  let globalImports = getSchemaImports options schemaPath
  let result = processClientDocument (options {globalImports}) schemaDoc (Just file) (pack moduleName)
  processDocument (isCheck ctx) hsPath result
  where
    hsPath = processFileName queryPath

handleServerService :: Context -> Service -> IO CommandResult
handleServerService ctx s@Service {name} = do
  (root, namespaces, files, globalImports) <- parseServiceData ctx s
  putStrLn ("\n build:" <> name)
  and <$> traverse (buildServer ctx (BuildConfig {..}) {root, namespaces}) files

buildServer :: Context -> BuildConfig -> FilePath -> IO CommandResult
buildServer ctx options path = do
  putStr ("  - " <> path <> "\n")
  file <- L.readFile path
  let moduleName = getModuleNameByPath (root options) hsPath
  let result = processServerDocument options moduleName file
  processDocument (isCheck ctx) hsPath result
  where
    hsPath = processFileName path

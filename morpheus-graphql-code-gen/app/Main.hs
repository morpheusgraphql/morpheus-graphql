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
    Source (..),
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
import Data.Morpheus.Internal.Ext (resultOr)
import qualified Data.Text.IO as TIO
import Data.Version (showVersion)
import qualified Paths_morpheus_graphql_code_gen as CLI
import Relude hiding (ByteString)
import System.Exit (ExitCode (..))
import System.FilePath (dropFileName, normalise, (</>))
import System.FilePath.Glob (glob)

currentVersion :: String
currentVersion = showVersion CLI.version

main :: IO ()
main = parseCLI >>= runApp

sourcesWithFallback :: [FilePath] -> IO [FilePath]
sourcesWithFallback [] = fmap (map dropFileName) (glob "**/code-gen.yaml")
sourcesWithFallback xs = pure xs

runApp :: App -> IO ()
runApp App {..}
  | version options = putStrLn currentVersion
  | otherwise = runOperation operations
  where
    runOperation About = putStrLn $ "Morpheus GraphQL CLI, version " <> currentVersion
    runOperation (Build source) = (sourcesWithFallback source) >>= processAll False
    runOperation (Check source) = (sourcesWithFallback source) >>= processAll True

data Context = Context
  { isCheck :: Bool,
    configDir :: FilePath
  }

type CommandResult = Bool

processAll :: Bool -> [FilePath] -> IO b
processAll check xs = do
  res <- traverse (scan . Context check) xs
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
getImports = concatMap optionImports . maybeToList

expandSource :: FilePath -> Source -> IO [Source]
expandSource root (Source p o) = do
  files <- glob $ normalise (root </> p)
  pure $ map (`Source` o) files

parseServiceData :: Context -> Service -> IO (FilePath, [Source], ServiceOptions)
parseServiceData ctx Service {source, includes, options} = do
  let root = normalise (configDir ctx </> source)
  let namespaces = maybe False optionNamespace options
  let externals = maybe mempty optionExternals options
  files <- concat <$> traverse (expandSource root) includes
  pure
    ( root,
      files,
      ServiceOptions namespaces (getImports options) externals
    )

getSchemaPath :: FilePath -> String -> Maybe Source -> IO Source
getSchemaPath root _ (Just Source {..}) = do
  pure Source {sourcePath = normalise $ root </> sourcePath, ..}
getSchemaPath _ name _ = fail $ "client service " <> name <> " should provide schema!"

handleClientService :: Context -> Service -> IO CommandResult
handleClientService ctx s@Service {name, schema} = do
  (root, files, buildOptions) <- parseServiceData ctx s
  putStrLn ("\n build:" <> name)
  schemaPath <- getSchemaPath root name schema
  let config = BuildConfig {..}
  (imports, globals) <- buildClientGlobals ctx config schemaPath
  let newConfig = config {buildOptions = buildOptions {optionImports = imports <> optionImports buildOptions}}
  and . (globals :) <$> traverse (buildClientQuery ctx newConfig schemaPath) files

buildClientGlobals :: Context -> BuildConfig -> Source -> IO ([Text], CommandResult)
buildClientGlobals ctx config src@Source {sourcePath} = do
  putStr ("  - " <> sourcePath <> "\n")
  schemaDoc <- readSchemaSource sourcePath
  let hsPath = processFileName sourcePath
  let moduleName = getModuleNameByPath (root config) hsPath
  let result = processClientDocument (localConfig config src) schemaDoc Nothing moduleName
  res <- processDocument (isCheck ctx) hsPath result
  pure ([moduleName | resultOr (const False) isJust result], res)

buildClientQuery :: Context -> BuildConfig -> Source -> Source -> IO CommandResult
buildClientQuery ctx config schemaPath querySrc = do
  let queryPath = sourcePath querySrc
  putStr ("  - " <> queryPath <> "\n")
  file <- TIO.readFile queryPath
  schemaDoc <- readSchemaSource (sourcePath schemaPath)
  let hsPath = processFileName queryPath
  let moduleName = getModuleNameByPath (root config) hsPath
  let result = processClientDocument (localConfig config querySrc) schemaDoc (Just file) moduleName
  processDocument (isCheck ctx) hsPath result

handleServerService :: Context -> Service -> IO CommandResult
handleServerService ctx s@Service {name} = do
  (root, files, buildOptions) <- parseServiceData ctx s
  putStrLn ("\n build:" <> name)
  and <$> traverse (buildServer ctx BuildConfig {..}) files

buildServer :: Context -> BuildConfig -> Source -> IO CommandResult
buildServer ctx config src = do
  let path = sourcePath src
  let hsPath = processFileName path
  putStr ("  - " <> path <> "\n")
  file <- L.readFile path
  let moduleName = getModuleNameByPath (root config) hsPath
  let result = processServerDocument (localConfig config src) moduleName file
  processDocument (isCheck ctx) hsPath (Just <$> result)

localConfig :: BuildConfig -> Source -> BuildConfig
localConfig (BuildConfig root ops) src =
  let options = sconcat (ops :| maybeToList (sourceOptions src))
   in BuildConfig root options

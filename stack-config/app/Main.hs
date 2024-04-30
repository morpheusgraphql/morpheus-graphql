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
import CLI.File (getModuleNameByPath, processFileName)
import qualified Data.ByteString.Lazy as L
  ( readFile,
  )
import qualified Data.Text.IO as TIO
import Data.Version (showVersion)
import qualified Paths_stack_config as CLI
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
  pure True

handleServerService :: Context -> Service -> IO CommandResult
handleServerService ctx s@Service {name} = do
  (root, files, buildOptions) <- parseServiceData ctx s
  putStrLn ("\n build:" <> name)
  pure True

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
    runOperation (Setup source) = processAll (scan . Context) source

data Context = Context {configDir :: FilePath}

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

handleClientService :: Context -> Service -> IO CommandResult
handleClientService ctx s@Service {name, schema} = do
  putStrLn ("\n build:" <> name)
  pure True

handleServerService :: Context -> Service -> IO CommandResult
handleServerService ctx s@Service {name} = do
  putStrLn ("\n build:" <> name)
  pure True

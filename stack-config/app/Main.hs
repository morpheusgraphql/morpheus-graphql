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
  ( Config,
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
    runOperation (Setup source) = do
      putStrLn "something"
      config <- readConfig ""
      putStrLn (show config)

data Context = Context {configDir :: FilePath}

type CommandResult = Bool

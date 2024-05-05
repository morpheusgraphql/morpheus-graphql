{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Config
  ( Config,
    genHie,
    readYaml,
    updateStack,
    writeYaml,
  )
import Data.Text (pack)
import Data.Version (showVersion)
import qualified Paths_stack_config as CLI
import Relude hiding (ByteString)

currentVersion :: String
currentVersion = showVersion CLI.version

main :: IO ()
main = parseCLI >>= runApp

runApp :: App -> IO ()
runApp App {..}
  | version options = putStrLn currentVersion
  | otherwise = runOperation operations
  where
    runOperation About = putStrLn $ "Stack Config CLI, version " <> currentVersion
    runOperation (Setup []) = setup "latest"
    runOperation (Setup (version : _)) = setup version

configPath :: FilePath
configPath = "./config/stack.yaml"

stackPath :: FilePath
stackPath = "./stack.yaml"

hiePath :: FilePath
hiePath = "./hie.yaml"

setup :: String -> IO ()
setup version = do
  config :: Config <- readYaml configPath
  writeYaml configPath config
  readYaml stackPath
    >>= updateStack (pack version) config
    >>= writeYaml stackPath
  genHie (pack stackPath) config >>= writeYaml hiePath
  putStrLn "Ok"

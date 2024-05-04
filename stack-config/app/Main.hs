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
    Stack,
    parseYaml,
    serializeYaml,
    updateStack,
  )
import Data.Aeson
import qualified Data.ByteString as L
  ( readFile,
    writeFile,
  )
import Data.Text (pack)
import Data.Version (showVersion)
import qualified Paths_stack_config as CLI
import Relude hiding (ByteString)

currentVersion :: String
currentVersion = showVersion CLI.version

main :: IO ()
main = parseCLI >>= runApp

readYaml :: (FromJSON a) => FilePath -> IO a
readYaml = L.readFile >=> parseYaml

writeYaml :: (ToJSON a) => FilePath -> a -> IO ()
writeYaml path = L.writeFile path . serializeYaml

runApp :: App -> IO ()
runApp App {..}
  | version options = putStrLn currentVersion
  | otherwise = runOperation operations
  where
    runOperation About = putStrLn $ "Morpheus GraphQL CLI, version " <> currentVersion
    runOperation (Setup []) = setup "latest"
    runOperation (Setup (version : _)) = setup version

configPath :: FilePath
configPath = "./config/stack.yaml"

stackPath :: String
stackPath = "./stack.yaml"

setup :: String -> IO ()
setup version = do
  config :: Config <- readYaml configPath
  writeYaml configPath config
  stack :: Stack <- readYaml stackPath
  updateStack (pack version) config stack >>= writeYaml stackPath
  putStrLn "Ok"
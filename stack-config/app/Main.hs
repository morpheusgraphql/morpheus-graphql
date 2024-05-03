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
import Data.Version (showVersion)
import qualified Paths_stack_config as CLI
import Relude hiding (ByteString)

currentVersion :: String
currentVersion = showVersion CLI.version

main :: IO ()
main = parseCLI >>= runApp

readYaml :: (FromJSON a) => FilePath -> IO a
readYaml = L.readFile >=> parseYaml

writeYaml :: (ToJSON a) => Bool -> FilePath -> a -> IO ()
writeYaml pretty path = L.writeFile path . serializeYaml pretty

runApp :: App -> IO ()
runApp App {..}
  | version options = putStrLn currentVersion
  | otherwise = runOperation operations
  where
    runOperation About = putStrLn $ "Morpheus GraphQL CLI, version " <> currentVersion
    runOperation (Setup _) = do
      putStrLn "something"
      let configPath = "./config/stack.yaml"
      config :: Config <- readYaml configPath
      writeYaml True configPath config
      stack :: Stack <- readYaml "./stack.yaml"
      updateStack "latest" config stack >>= writeYaml True "./stack.yaml"
      putStrLn (show stack)

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
import Config (Config, parseYaml, serializeYaml)
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

readConfig :: FilePath -> IO Config
readConfig = L.readFile >=> parseYaml

writeConfig :: FilePath -> Config -> IO ()
writeConfig path = L.writeFile path . serializeYaml

runApp :: App -> IO ()
runApp App {..}
  | version options = putStrLn currentVersion
  | otherwise = runOperation operations
  where
    runOperation About = putStrLn $ "Morpheus GraphQL CLI, version " <> currentVersion
    runOperation (Setup _) = do
      putStrLn "something"
      let configPath = "./config/stack.yaml"
      config <- readConfig configPath
      writeConfig configPath config
      putStrLn (show config)

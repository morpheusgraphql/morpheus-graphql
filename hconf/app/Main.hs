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
import Data.Text (pack)
import Data.Version (showVersion)
import HConf
  ( Config,
    checkPackages,
    genHie,
    readYaml,
    updateStack,
    writeYaml,
  )
import qualified Paths_hconf as CLI
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

hconfPath :: FilePath
hconfPath = "./hconf.yaml"

stackPath :: FilePath
stackPath = "./stack.yaml"

hiePath :: FilePath
hiePath = "./hie.yaml"

setup :: String -> IO ()
setup version = do
  config :: Config <- readYaml hconfPath
  writeYaml hconfPath config
  readYaml stackPath
    >>= updateStack (pack version) config
    >>= writeYaml stackPath
  genHie (pack stackPath) config >>= writeYaml hiePath
  checkPackages config
  putStrLn "Ok"

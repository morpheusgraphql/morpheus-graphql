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
import HConf (SetupPath (..), setup)
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
    runOperation (Setup []) = setup path "latest"
    runOperation (Setup (version : _)) = setup path version

path =
  SetupPath
    { hconf = "./hconf.yaml",
      hie = "./hie.yaml",
      stack = "./stack.yaml"
    }

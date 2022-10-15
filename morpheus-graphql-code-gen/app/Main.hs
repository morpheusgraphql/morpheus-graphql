{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main,
  )
where

import CLI.Commands
import CLI.File
import qualified Data.ByteString.Lazy as L
  ( readFile,
  )
import Data.Morpheus.CodeGen
  ( CodeGenConfig (..),
    PrinterConfig (..),
    parseServerTypeDefinitions,
    printServerTypeDefinitions,
  )
import Data.Version (showVersion)
import qualified Paths_morpheus_graphql_code_gen as CLI
import Relude hiding (ByteString)

currentVersion :: String
currentVersion = showVersion CLI.version

main :: IO ()
main = defaultParser >>= runApp

runApp :: App -> IO ()
runApp App {..}
  | version options = putStrLn currentVersion
  | otherwise = runOperation operations
  where
    runOperation About = putStrLn $ "Morpheus GraphQL CLI, version " <> currentVersion
    runOperation Build {source} = traverse_ (processFile options) source

processFile :: Options -> FilePath -> IO ()
processFile Options {root, namespaces} path =
  print (path, hsPath)
    >> L.readFile path
    >>= saveDocument hsPath
      . fmap
        ( printServerTypeDefinitions
            PrinterConfig
              { moduleName = getModuleNameByPath root hsPath
              }
        )
      . parseServerTypeDefinitions CodeGenConfig {namespace = namespaces}
  where
    hsPath = processFileName path

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CLI.Generator (processFile) where

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
import Relude hiding (ByteString)

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

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CLI.Generator (processFile) where

import CLI.Commands
  ( Options (..),
  )
import CLI.File
  ( getModuleNameByPath,
    processFileName,
    saveDocument,
  )
import qualified Data.ByteString.Lazy as L
  ( readFile,
  )
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus.CodeGen
  ( CodeGenConfig (..),
    PrinterConfig (..),
    parseServerTypeDefinitions,
    printServerTypeDefinitions,
  )
import Data.Morpheus.Internal.Ext (GQLResult)
import Relude hiding (ByteString)

process :: Options -> FilePath -> ByteString -> GQLResult ByteString
process Options {root, namespaces} hsPath =
  fmap
    ( printServerTypeDefinitions
        PrinterConfig
          { moduleName = getModuleNameByPath root hsPath
          }
    )
    . parseServerTypeDefinitions CodeGenConfig {namespace = namespaces}

processFile :: Options -> FilePath -> IO ()
processFile options@Options {root, namespaces} path = do
  file <- L.readFile path
  saveDocument hsPath (process options hsPath file)
  where
    hsPath = processFileName path

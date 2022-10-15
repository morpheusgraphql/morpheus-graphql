{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CLI.Generator (processDocument) where

import CLI.Commands
  ( Options (..),
  )
import CLI.File
  ( getModuleNameByPath,
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

processDocument :: Options -> FilePath -> ByteString -> GQLResult ByteString
processDocument Options {root, namespaces} hsPath =
  fmap
    ( printServerTypeDefinitions
        PrinterConfig
          { moduleName = getModuleNameByPath root hsPath
          }
    )
    . parseServerTypeDefinitions CodeGenConfig {namespace = namespaces}

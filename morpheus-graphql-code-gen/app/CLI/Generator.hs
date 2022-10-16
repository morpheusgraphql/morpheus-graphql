{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CLI.Generator
  ( processServerDocument,
    processClientDocument,
  )
where

import CLI.Commands
  ( BuildOptions (..),
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

processServerDocument :: BuildOptions -> FilePath -> ByteString -> GQLResult ByteString
processServerDocument BuildOptions {root, namespaces} hsPath =
  fmap
    ( printServerTypeDefinitions
        PrinterConfig
          { moduleName = getModuleNameByPath root hsPath
          }
    )
    . parseServerTypeDefinitions CodeGenConfig {namespace = namespaces}

processClientDocument :: BuildOptions -> FilePath -> ByteString -> GQLResult ByteString
processClientDocument BuildOptions {root, namespaces} hsPath =
  fmap
    ( printServerTypeDefinitions
        PrinterConfig
          { moduleName = getModuleNameByPath root hsPath
          }
    )
    . parseServerTypeDefinitions CodeGenConfig {namespace = namespaces}

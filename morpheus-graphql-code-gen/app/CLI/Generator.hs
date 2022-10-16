{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Data.ByteString.Lazy.Char8 (ByteString, pack)
import Data.Morpheus.Client
  ( parseClientTypeDeclarations,
    printClientTypeDeclarations,
  )
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

processClientDocument :: BuildOptions -> FilePath -> Text -> GQLResult ByteString
processClientDocument BuildOptions {root, namespaces} hsPath query =
  pack . show . printClientTypeDeclarations <$> parseClientTypeDeclarations undefined (Just query)

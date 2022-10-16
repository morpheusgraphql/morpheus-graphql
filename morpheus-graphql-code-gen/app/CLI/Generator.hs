{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
  ( SchemaSource,
    parseClientTypeDeclarations,
  )
import Data.Morpheus.CodeGen
  ( CodeGenConfig (..),
    PrinterConfig (..),
    parseServerTypeDefinitions,
    printServerTypeDefinitions,
  )
import Data.Morpheus.CodeGen.Internal.AST
import Data.Morpheus.Internal.Ext (GQLResult)
import Prettyprinter
import Relude hiding (ByteString)

processServerDocument :: BuildOptions -> String -> ByteString -> GQLResult ByteString
processServerDocument BuildOptions {..} moduleName =
  fmap
    ( printServerTypeDefinitions
        PrinterConfig
          { moduleName
          }
    )
    . parseServerTypeDefinitions CodeGenConfig {namespace = namespaces}

processClientDocument :: BuildOptions -> SchemaSource -> Maybe Text -> Text -> GQLResult ByteString
processClientDocument BuildOptions {} schema query moduleName = do
  types <- parseClientTypeDeclarations schema query
  let moduleDef =
        ModuleDefinition
          { moduleName,
            imports = [],
            extensions = [],
            types
          }
  pure $ pack $ show $ pretty moduleDef

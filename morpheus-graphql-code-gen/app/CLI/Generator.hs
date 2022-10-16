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
import Relude hiding (ByteString, print)

processServerDocument :: BuildOptions -> String -> ByteString -> GQLResult ByteString
processServerDocument BuildOptions {..} moduleName =
  fmap
    ( printServerTypeDefinitions
        PrinterConfig
          { moduleName
          }
    )
    . parseServerTypeDefinitions CodeGenConfig {namespace = namespaces}

processClientDocument ::
  BuildOptions ->
  SchemaSource ->
  Maybe Text ->
  [(Text, [Text])] ->
  Text ->
  GQLResult ByteString
processClientDocument BuildOptions {} schema query globals moduleName = do
  types <- parseClientTypeDeclarations schema query
  let moduleDef =
        ModuleDefinition
          { moduleName,
            imports =
              [ ("Data.Text", ["Text"]),
                ("GHC.Generics", ["Generic"]),
                ("Globals.GQLScalars", ["*"])
              ]
                <> globals,
            extensions =
              [ "DeriveGeneric",
                "DuplicateRecordFields"
              ],
            types
          }
  pure $ print moduleDef

print :: Pretty a => a -> ByteString
print = pack . show . pretty
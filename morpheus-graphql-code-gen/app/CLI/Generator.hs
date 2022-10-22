{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CLI.Generator
  ( processServerDocument,
    processClientDocument,
    BuildConfig (..),
  )
where

import Data.ByteString.Lazy.Char8 (ByteString, pack)
import Data.Morpheus.Client
  ( SchemaSource,
    parseClientTypeDeclarations,
  )
import Data.Morpheus.Client.CodeGen.AST (ClientDeclaration (..), DERIVING_MODE (SCALAR_MODE))
import Data.Morpheus.CodeGen
  ( CodeGenConfig (..),
    parseServerTypeDefinitions,
  )
import Data.Morpheus.CodeGen.Internal.AST
import Data.Morpheus.Internal.Ext (GQLResult)
import Prettyprinter
import Relude hiding (ByteString, print)

data BuildConfig = BuildConfig
  { root :: String,
    namespaces :: Bool,
    globalImports :: [Text]
  }
  deriving (Show)

processServerDocument :: BuildConfig -> Text -> ByteString -> GQLResult ByteString
processServerDocument BuildConfig {..} moduleName schema = do
  types <- parseServerTypeDefinitions CodeGenConfig {namespace = namespaces} schema
  pure $
    print $
      ModuleDefinition
        { moduleName,
          imports =
            [ ("Data.Data", ["Typeable"]),
              ("Data.Morpheus.Kind", ["TYPE"]),
              ("Data.Morpheus.Types", ["*"]),
              ("Data.Morpheus", []),
              ("Data.Text", ["Text"]),
              ("GHC.Generics", ["Generic"])
            ]
              <> map (,["*"]) globalImports,
          extensions =
            [ "DataKinds",
              "DeriveGeneric",
              "DuplicateRecordFields",
              "OverloadedStrings",
              "TypeFamilies",
              "{-# HLINT ignore \"Use camelCase\" #-}",
              "{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}"
            ],
          types
        }

notScalars :: ClientDeclaration -> Bool
notScalars (InstanceDeclaration SCALAR_MODE _) = False
notScalars _ = True

processClientDocument ::
  BuildConfig ->
  SchemaSource ->
  Maybe Text ->
  Text ->
  GQLResult ByteString
processClientDocument BuildConfig {..} schema query moduleName = do
  types <- filter notScalars <$> parseClientTypeDeclarations schema query
  let moduleDef =
        ModuleDefinition
          { moduleName,
            imports =
              [("Data.Morpheus.Client.CodeGen.Internal", ["*"])]
                <> map (,["*"]) globalImports,
            extensions =
              [ "DeriveGeneric",
                "DuplicateRecordFields",
                "LambdaCase",
                "OverloadedStrings",
                "TypeFamilies"
              ],
            types
          }
  pure $ print moduleDef

print :: Pretty a => a -> ByteString
print = pack . show . pretty

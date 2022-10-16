{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.CodeGen.Server.Printing.Document
  ( renderDocument,
  )
where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus.CodeGen.Internal.AST (CodeGenTypeName (..))
import Data.Morpheus.CodeGen.Printer
  ( Printer (..),
    ignore,
    optional,
    renderExtension,
    renderImport,
    unpack,
    (.<>),
  )
import Data.Morpheus.CodeGen.Server.Internal.AST
  ( GQLTypeDefinition (..),
    InterfaceDefinition (..),
    Kind (..),
    ModuleDefinition (..),
    ServerDeclaration (..),
    ServerDirectiveUsage (..),
  )
import Data.Text
  ( pack,
  )
import qualified Data.Text.Lazy as LT
  ( fromStrict,
  )
import Data.Text.Lazy.Encoding (encodeUtf8)
import Prettyprinter
  ( Doc,
    align,
    indent,
    line,
    pretty,
    punctuate,
    tupled,
    vsep,
    (<+>),
  )
import Relude hiding (ByteString, encodeUtf8, optional, print)

renderDocument :: String -> [ServerDeclaration] -> ByteString
renderDocument moduleName types =
  encodeUtf8 $
    LT.fromStrict $
      show $
        renderModuleDefinition
          ModuleDefinition
            { moduleName = pack moduleName,
              imports =
                [ ("Data.Data", ["Typeable"]),
                  ("Data.Morpheus.Kind", ["TYPE"]),
                  ("Data.Morpheus.Types", ["*"]),
                  ("Data.Morpheus", []),
                  ("Data.Text", ["Text"]),
                  ("GHC.Generics", ["Generic"]),
                  ("Globals.GQLScalars", ["*"])
                ],
              extensions =
                [ "DeriveGeneric",
                  "TypeFamilies",
                  "OverloadedStrings",
                  "DataKinds",
                  "DuplicateRecordFields"
                ],
              types
            }

renderModuleDefinition :: ModuleDefinition -> Doc n
renderModuleDefinition
  ModuleDefinition
    { extensions,
      moduleName,
      imports,
      types
    } =
    vsep
      (map renderExtension extensions)
      <> "{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}"
      <> line
      <> line
      <> "{-# HLINT ignore \"Use camelCase\" #-}"
      <> line
      <> line
      <> "module"
      <+> pretty moduleName
      <+> "where"
        <> line
        <> line
        <> vsep (map renderImport imports)
        <> line
        <> line
        <> either (error . show) id (renderTypes types)

type Result = Either Text

renderTypes :: [ServerDeclaration] -> Either Text (Doc ann)
renderTypes = fmap vsep . traverse render

class RenderType a where
  render :: a -> Result (Doc ann)

instance RenderType ServerDeclaration where
  render (InterfaceType InterfaceDefinition {..}) =
    pure $
      "type"
        <+> ignore (print aliasName)
        <+> "m"
        <+> "="
        <+> "TypeGuard"
        <+> unpack (print interfaceName .<> "m")
        <+> unpack (print unionName .<> "m")
  -- TODO: on scalar we should render user provided type
  render ScalarType {scalarTypeName} =
    pure $ "type" <+> ignore (print scalarTypeName) <+> "= Int"
  render (DataType cgType) = pure (pretty cgType)
  render (GQLTypeInstance gqlType) = pure $ renderGQLType gqlType
  render (GQLDirectiveInstance _) = fail "not supported"

renderTypeableConstraints :: [Text] -> Doc n
renderTypeableConstraints xs = tupled (map (("Typeable" <+>) . pretty) xs) <+> "=>"

renderGQLType :: GQLTypeDefinition -> Doc ann
renderGQLType gql@GQLTypeDefinition {..}
  | gqlKind == Scalar = ""
  | otherwise =
    "instance"
      <> optional renderTypeableConstraints (typeParameters gqlTarget)
      <+> "GQLType"
      <+> typeHead
      <+> "where"
        <> line
        <> indent 2 (vsep (renderMethods typeHead gql))
  where
    typeHead = unpack (print gqlTarget)

renderMethods :: Doc n -> GQLTypeDefinition -> [Doc n]
renderMethods typeHead GQLTypeDefinition {..} =
  ["type KIND" <+> typeHead <+> "=" <+> pretty gqlKind]
    <> ["directives _=" <+> renderDirectiveUsages gqlTypeDirectiveUses | not (null gqlTypeDirectiveUses)]

renderDirectiveUsages :: [ServerDirectiveUsage] -> Doc n
renderDirectiveUsages = align . vsep . punctuate " <>" . map pretty

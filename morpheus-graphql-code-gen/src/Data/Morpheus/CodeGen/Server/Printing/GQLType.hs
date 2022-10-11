{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.CodeGen.Server.Printing.GQLType
  ( renderGQLType,
  )
where

import Data.Morpheus.CodeGen.Internal.AST
  ( CodeGenTypeName (..),
  )
import Data.Morpheus.CodeGen.Printer
  ( Printer (print),
    optional,
    unpack,
  )
import Data.Morpheus.CodeGen.Server.Internal.AST
  ( GQLTypeDefinition (..),
    ServerDirectiveUsage (..),
    TypeKind,
  )
import Prettyprinter
import Relude hiding (optional, print, show)
import Prelude (show)

renderTypeableConstraints :: [Text] -> Doc n
renderTypeableConstraints xs = tupled (map (("Typeable" <+>) . pretty) xs) <+> "=>"

defineTypeOptions :: Maybe (TypeKind, Text) -> [Doc n]
defineTypeOptions (Just (kind, tName)) = ["typeOptions _ = dropNamespaceOptions" <+> "(" <> pretty (show kind) <> ")" <+> pretty (show tName)]
defineTypeOptions _ = []

renderGQLType :: GQLTypeDefinition -> Doc ann
renderGQLType gql@GQLTypeDefinition {..} =
  "instance"
    <> optional renderTypeableConstraints (typeParameters gqlTarget)
    <+> "GQLType"
    <+> typeHead
    <+> "where"
      <> line
      <> indent 2 (vsep (renderMethods typeHead gql <> defineTypeOptions dropNamespace))
  where
    typeHead = unpack (print gqlTarget)

renderMethods :: Doc n -> GQLTypeDefinition -> [Doc n]
renderMethods typeHead GQLTypeDefinition {..} =
  ["type KIND" <+> typeHead <+> "=" <+> pretty gqlKind]
    <> ["directives _=" <+> renderDirectiveUsages gqlTypeDirectiveUses | not (null gqlTypeDirectiveUses)]

renderDirectiveUsages :: [ServerDirectiveUsage] -> Doc n
renderDirectiveUsages = align . vsep . punctuate " <>" . map renderDirectiveUsage

renderDirectiveUsage :: ServerDirectiveUsage -> Doc n
renderDirectiveUsage (TypeDirectiveUsage value) = "typeDirective" <+> pretty value
renderDirectiveUsage (FieldDirectiveUsage place value) = "fieldDirective" <+> pretty (show place) <+> pretty value
renderDirectiveUsage (EnumDirectiveUsage place value) = "enumDirective" <+> pretty (show place) <+> pretty value

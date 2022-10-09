{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.CodeGen.Server.Printing.GQLType
  ( renderGQLType,
  )
where

import Data.Morpheus.CodeGen.Printer
  ( optional,
    parametrizedType,
  )
import Data.Morpheus.CodeGen.Server.Internal.AST
  ( GQLTypeDefinition (..),
    ServerDirectiveUsage (..),
    ServerTypeDefinition (..),
    TypeKind,
  )
import Prettyprinter
import Relude hiding (optional, show)
import Prelude (show)

renderTypeableConstraints :: [Text] -> Doc n
renderTypeableConstraints xs = tupled (map (("Typeable" <+>) . pretty) xs) <+> "=>"

defineTypeOptions :: Maybe (TypeKind, Text) -> [Doc n]
defineTypeOptions (Just (kind, tName)) = ["typeOptions _ = dropNamespaceOptions" <+> "(" <> pretty (show kind) <> ")" <+> pretty (show tName)]
defineTypeOptions _ = []

renderGQLType :: ServerTypeDefinition -> Doc n
renderGQLType ServerTypeDefinition {..} =
  "instance"
    <> optional renderTypeableConstraints typeParameters
    <+> "GQLType"
    <+> typeHead
    <+> "where"
      <> line
      <> indent 2 (vsep (methods <> options))
  where
    methods = renderMethods typeHead typeGQLType
    options = defineTypeOptions (typeGQLType >>= dropNamespace)
    typeHead =
      if null typeParameters
        then parametrizedType tName typeParameters
        else tupled (pure $ parametrizedType tName typeParameters)
renderGQLType _ = ""

renderMethods :: Doc n -> Maybe GQLTypeDefinition -> [Doc n]
renderMethods _ Nothing = []
renderMethods
  typeHead
  (Just GQLTypeDefinition {..}) =
    ["type KIND" <+> typeHead <+> "=" <+> pretty gqlKind]
      <> ["directives _=" <+> renderDirectiveUsages gqlTypeDirectiveUses | not (null gqlTypeDirectiveUses)]

renderDirectiveUsages :: [ServerDirectiveUsage] -> Doc n
renderDirectiveUsages = align . vsep . punctuate " <>" . map renderDirectiveUsage

renderDirectiveUsage :: ServerDirectiveUsage -> Doc n
renderDirectiveUsage (TypeDirectiveUsage value) = "typeDirective" <+> pretty value
renderDirectiveUsage (FieldDirectiveUsage place value) = "fieldDirective" <+> pretty (show place) <+> pretty value
renderDirectiveUsage (EnumDirectiveUsage place value) = "enumDirective" <+> pretty (show place) <+> pretty value

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.CodeGen.Printing.GQLType
  ( renderGQLType,
  )
where

import Data.Morpheus.CodeGen.Internal.AST
  ( GQLTypeDefinition (..),
    Kind (..),
    ServerDirectiveUsage (..),
    ServerTypeDefinition (..),
    TypeKind,
  )
import Data.Morpheus.CodeGen.Printing.Terms
  ( optional,
    parametrizedType,
  )
import Prettyprinter
import Relude hiding (optional, show)
import Prelude (show)

renderTypeableConstraints :: [Text] -> Doc n
renderTypeableConstraints xs = tupled (map (("Typeable" <+>) . pretty) xs) <+> "=>"

defineTypeOptions :: Bool -> Text -> TypeKind -> Doc n
defineTypeOptions namespaces tName kind
  | namespaces = "typeOptions _ = dropNamespaceOptions " <> pretty tName <> " (" <> pretty (show kind) <> ")"
  | otherwise = "typeOptions _ options = options"

renderGQLType :: ServerTypeDefinition -> Doc n
renderGQLType ServerTypeDefinition {..} =
  "instance"
    <> optional renderTypeableConstraints typeParameters
    <+> "GQLType"
    <+> typeHead
    <+> "where"
      <> line
      <> indent 2 (vsep (renderMethods typeHead typeGQLType <> [options]))
  where
    options = defineTypeOptions False tName tKind
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
    ["type KIND" <+> typeHead <+> "=" <+> renderKind gqlKind]
      <> ["directives _=" <+> renderDirectiveUsages gqlTypeDirectiveUses | not (null gqlTypeDirectiveUses)]

renderDirectiveUsages :: [ServerDirectiveUsage] -> Doc n
renderDirectiveUsages = align . vsep . punctuate " <>" . map renderDirectiveUsage

renderDirectiveUsage :: ServerDirectiveUsage -> Doc n
renderDirectiveUsage (TypeDirectiveUsage value) = "typeDirective" <+> pretty value
renderDirectiveUsage (FieldDirectiveUsage place value) = "fieldDirective" <+> pretty (show place) <+> pretty value
renderDirectiveUsage (EnumDirectiveUsage place value) = "enumDirective" <+> pretty (show place) <+> pretty value

renderKind :: Kind -> Doc n
renderKind Type = "TYPE"
renderKind Scalar = "SCALAR"

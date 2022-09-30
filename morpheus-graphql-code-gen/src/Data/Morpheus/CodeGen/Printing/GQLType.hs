{-# LANGUAGE NamedFieldPuns #-}
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
    ServerTypeDefinition (..),
    TypeKind,
  )
import Data.Morpheus.CodeGen.Printing.Terms
  ( optional,
    parametrizedType,
  )
import Prettyprinter
  ( Doc,
    Pretty (pretty),
    indent,
    line,
    tupled,
    vsep,
    (<+>),
  )
import Relude hiding (optional, show)
import Prelude (show)

renderTypeableConstraints :: [Text] -> Doc n
renderTypeableConstraints xs = tupled (map (("Typeable" <+>) . pretty) xs) <+> "=>"

defineTypeOptions :: Bool -> Text -> TypeKind -> Doc n
defineTypeOptions namespaces tName kind
  | namespaces = "typeOptions _ = dropNamespaceOptions " <> pretty tName <> " (" <> pretty (show kind) <> ")"
  | otherwise = "typeOptions _ = id"

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
  ( Just
      GQLTypeDefinition
        { gqlTypeDescription,
          gqlTypeDescriptions,
          gqlKind
        }
    ) =
    ["type KIND" <+> typeHead <+> "=" <+> renderKind gqlKind]
      <> ["description _ =" <+> pretty (show gqlTypeDescription) | not (null gqlTypeDescription)]
      <> ["getDescriptions _ =" <+> pretty (show gqlTypeDescriptions) | not (null gqlTypeDescriptions)]

renderKind :: Kind -> Doc n
renderKind Type = "TYPE"
renderKind Scalar = "SCALAR"

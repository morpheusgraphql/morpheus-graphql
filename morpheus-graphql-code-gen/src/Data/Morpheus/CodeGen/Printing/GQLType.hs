{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
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
    TypeName,
  )
import Data.Morpheus.CodeGen.Printing.Terms
  ( optional,
    parametrizedType,
    renderName,
  )
import Data.Text.Prettyprint.Doc
  ( (<+>),
    Doc,
    Pretty (pretty),
    indent,
    line,
    tupled,
    vsep,
  )
import Relude hiding (optional, show)
import Prelude (show)

renderTypeableConstraints :: [TypeName] -> Doc n
renderTypeableConstraints xs = tupled (map (("Typeable" <+>) . renderName) xs) <+> "=>"

-- TODO: fill namespace options
defineTypeOptions :: TypeName -> TypeKind -> Doc n
defineTypeOptions tName kind = ""

renderGQLType :: ServerTypeDefinition -> Doc n
renderGQLType ServerTypeDefinition {tName, typeParameters, tKind, gql} =
  "instance"
    <> optional renderTypeableConstraints typeParameters
    <+> "GQLType"
    <+> typeHead
    <+> "where"
    <> line
    <> indent 2 (vsep (renderMethods typeHead gql <> [options]))
  where
    options = defineTypeOptions tName tKind
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

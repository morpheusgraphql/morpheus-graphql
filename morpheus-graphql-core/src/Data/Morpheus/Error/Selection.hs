{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Error.Selection
  ( unknownSelectionField,
    subfieldsNotSelected,
    hasNoSubfields,
  )
where

import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    GQLError,
    Position,
    Ref (..),
    TypeDefinition (..),
    TypeName,
    VALID,
    at,
    msg,
  )
import Data.Semigroup ((<>))

-- GQL: "Field \"default\" must not have a selection since type \"String!\" has no subfields."
hasNoSubfields :: Ref FieldName -> TypeDefinition s VALID -> GQLError
hasNoSubfields (Ref selectionName position) TypeDefinition {typeName} = text `at` position
  where
    text =
      "Field "
        <> msg selectionName
        <> " must not have a selection since type "
        <> msg typeName
        <> " has no subfields."

unknownSelectionField :: TypeName -> Ref FieldName -> GQLError
unknownSelectionField typeName Ref {refName, refPosition} = text `at` refPosition
  where
    text =
      "Cannot query field " <> msg refName
        <> " on type "
        <> msg typeName
        <> "."

-- GQL:: Field \"hobby\" of type \"Hobby!\" must have a selection of subfields. Did you mean \"hobby { ... }\"?
subfieldsNotSelected :: FieldName -> TypeName -> Position -> GQLError
subfieldsNotSelected fieldName typeName position = text `at` position
  where
    text =
      "Field " <> msg fieldName <> " of type "
        <> msg typeName
        <> " must have a selection of subfields"

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
    Position,
    Ref (..),
    TypeDefinition (..),
    TypeName,
    VALID,
    ValidationError,
    at,
    msgValidation,
  )
import Data.Semigroup ((<>))

-- GQL: "Field \"default\" must not have a selection since type \"String!\" has no subfields."
hasNoSubfields :: Ref FieldName -> TypeDefinition s VALID -> ValidationError
hasNoSubfields (Ref selectionName position) TypeDefinition {typeName} = text `at` position
  where
    text =
      "Field "
        <> msgValidation selectionName
        <> " must not have a selection since type "
        <> msgValidation typeName
        <> " has no subfields."

unknownSelectionField :: TypeName -> Ref FieldName -> ValidationError
unknownSelectionField typeName Ref {refName, refPosition} = text `at` refPosition
  where
    text =
      "Cannot query field " <> msgValidation refName
        <> " on type "
        <> msgValidation typeName
        <> "."

-- GQL:: Field \"hobby\" of type \"Hobby!\" must have a selection of subfields. Did you mean \"hobby { ... }\"?
subfieldsNotSelected :: FieldName -> TypeName -> Position -> ValidationError
subfieldsNotSelected fieldName typeName position = text `at` position
  where
    text =
      "Field " <> msgValidation fieldName <> " of type "
        <> msgValidation typeName
        <> " must have a selection of subfields"

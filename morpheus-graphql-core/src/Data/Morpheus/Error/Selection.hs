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
    ValidationError (..),
    msg,
  )
import Data.Semigroup ((<>))

-- GQL: "Field \"default\" must not have a selection since type \"String!\" has no subfields."
hasNoSubfields :: Ref -> TypeDefinition s VALID -> ValidationError
hasNoSubfields (Ref selectionName position) TypeDefinition {typeName} = ValidationError text [position]
  where
    text =
      "Field "
        <> msg selectionName
        <> " must not have a selection since type "
        <> msg typeName
        <> " has no subfields."

unknownSelectionField :: TypeName -> Ref -> ValidationError
unknownSelectionField typeName Ref {refName, refPosition} = ValidationError text [refPosition]
  where
    text =
      "Cannot query field " <> msg refName
        <> " on type "
        <> msg typeName
        <> "."

-- GQL:: Field \"hobby\" of type \"Hobby!\" must have a selection of subfields. Did you mean \"hobby { ... }\"?
subfieldsNotSelected :: FieldName -> TypeName -> Position -> ValidationError
subfieldsNotSelected fieldName typeName position = ValidationError text [position]
  where
    text =
      "Field " <> msg fieldName <> " of type "
        <> msg typeName
        <> " must have a selection of subfields"

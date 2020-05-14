{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Selection
  ( unknownSelectionField,
    subfieldsNotSelected,
    hasNoSubfields,
  )
where

import Data.Morpheus.Error.Utils (errorMessage)
import Data.Morpheus.Types.Internal.AST.Base
  ( FieldName,
    GQLErrors,
    Position,
    Ref (..),
    TypeName,
    msg,
  )
import Data.Semigroup ((<>))

-- GQL: "Field \"default\" must not have a selection since type \"String!\" has no subfields."
hasNoSubfields :: Ref -> TypeName -> GQLErrors
hasNoSubfields (Ref selectionName position) typeName = errorMessage position text
  where
    text =
      "Field "
        <> msg selectionName
        <> " must not have a selection since type "
        <> msg typeName
        <> " has no subfields."

unknownSelectionField :: TypeName -> Ref -> GQLErrors
unknownSelectionField typeName Ref {refName, refPosition} = errorMessage refPosition text
  where
    text =
      "Cannot query field " <> msg refName
        <> " on type "
        <> msg typeName
        <> "."

-- GQL:: Field \"hobby\" of type \"Hobby!\" must have a selection of subfields. Did you mean \"hobby { ... }\"?
subfieldsNotSelected :: FieldName -> TypeName -> Position -> GQLErrors
subfieldsNotSelected fieldName typeName position = errorMessage position text
  where
    text =
      "Field " <> msg fieldName <> " of type "
        <> msg typeName
        <> " must have a selection of subfields"

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Error.Document.Interface
  ( unknownInterface,
    PartialImplements (..),
    ImplementsError (..),
    Place (..),
  )
where

import Data.Maybe (Maybe (..))
import Data.Morpheus.Types.Internal.AST.Base
  ( FieldName (..),
    TypeName (..),
    ValidationError,
    msgValidation,
  )
import Data.Morpheus.Types.Internal.AST.Type (TypeRef)
import Data.Morpheus.Types.Internal.Validation.SchemaValidator
  ( Field (..),
    Interface (..),
    renderField,
  )
import Data.Semigroup ((<>))

unknownInterface :: TypeName -> ValidationError
unknownInterface name = "Unknown Interface " <> msgValidation name <> "."

data ImplementsError
  = UnexpectedType
      { expectedType :: TypeRef,
        foundType :: TypeRef
      }
  | Missing

data Place = Place
  { fieldname :: TypeName,
    typename :: FieldName,
    fieldArg :: Maybe (FieldName, TypeName)
  }

class PartialImplements ctx where
  partialImplements :: ctx -> ImplementsError -> ValidationError

instance PartialImplements (Interface, FieldName) where
  partialImplements (Interface interfaceName typename, fieldname) errorType =
    "Interface field "
      <> renderField interfaceName fieldname Nothing
      <> detailedMessage errorType
    where
      detailedMessage UnexpectedType {expectedType, foundType} =
        " expects type "
          <> msgValidation expectedType
          <> " but "
          <> renderField typename fieldname Nothing
          <> " is type "
          <> msgValidation foundType
          <> "."
      detailedMessage Missing =
        " expected but "
          <> msgValidation typename
          <> " does not provide it."

-- Interface field TestInterface.name expected but User does not provide it.
-- Interface field TestInterface.name expects type String! but User.name is type Int!.

instance PartialImplements (Interface, Field) where
  partialImplements (Interface interfaceName typename, Field fieldname argName) errorType =
    "Interface field argument "
      <> renderField interfaceName fieldname (Just argName)
      <> detailedMessage errorType
    where
      detailedMessage UnexpectedType {expectedType, foundType} =
        " expects type"
          <> msgValidation expectedType
          <> " but "
          <> renderField typename fieldname (Just argName)
          <> " is type "
          <> msgValidation foundType
          <> "."
      detailedMessage Missing =
        " expected but "
          <> renderField typename fieldname Nothing
          <> " does not provide it."

-- Interface field argument TestInterface.name(id:) expected but User.name does not provide it.
-- Interface field argument TestInterface.name(id:) expects type ID but User.name(id:) is type String.

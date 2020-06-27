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
import Data.Morpheus.Error.Utils (globalErrorMessage)
import Data.Morpheus.Types.Internal.AST.Base
  ( FieldName (..),
    GQLError (..),
    GQLErrors,
    TypeName (..),
    TypeRef,
    msg,
  )
import Data.Morpheus.Types.Internal.Validation.SchemaValidator
  ( Field (..),
    Interface (..),
    renderField,
  )
import Data.Semigroup ((<>))

unknownInterface :: TypeName -> GQLErrors
unknownInterface name = globalErrorMessage message
  where
    message = "Unknown Interface " <> msg name <> "."

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
  partialImplements :: ctx -> ImplementsError -> GQLErrors

instance PartialImplements (Interface, FieldName) where
  partialImplements (Interface interfaceName typename, fieldname) errorType =
    [ GQLError
        { message = message,
          locations = []
        }
    ]
    where
      message =
        "Interface field "
          <> renderField interfaceName fieldname Nothing
          <> detailedMessage errorType
      detailedMessage UnexpectedType {expectedType, foundType} =
        " expects type "
          <> msg expectedType
          <> " but "
          <> renderField typename fieldname Nothing
          <> " is type "
          <> msg foundType
          <> "."
      detailedMessage Missing =
        " expected but "
          <> msg typename
          <> " does not provide it."

-- Interface field TestInterface.name expected but User does not provide it.
-- Interface field TestInterface.name expects type String! but User.name is type Int!.

instance PartialImplements (Interface, Field) where
  partialImplements (Interface interfaceName typename, Field fieldname argName) errorType =
    [ GQLError
        { message = message,
          locations = []
        }
    ]
    where
      --
      message =
        "Interface field argument "
          <> renderField interfaceName fieldname (Just argName)
          <> detailedMessage errorType
      detailedMessage UnexpectedType {expectedType, foundType} =
        " expects type"
          <> msg expectedType
          <> " but "
          <> renderField typename fieldname (Just argName)
          <> " is type "
          <> msg foundType
          <> "."
      detailedMessage Missing =
        " expected but "
          <> renderField typename fieldname Nothing
          <> " does not provide it."

-- Interface field argument TestInterface.name(id:) expected but User.name does not provide it.
-- Interface field argument TestInterface.name(id:) expects type ID but User.name(id:) is type String.

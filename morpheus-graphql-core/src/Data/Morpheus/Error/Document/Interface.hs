{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Document.Interface
  ( unknownInterface,
    PartialImplements (..),
    ImplementsError (..),
    Place (..),
  )
where

import Data.Morpheus.Error.Utils (globalErrorMessage)
import Data.Morpheus.Types.Internal.AST.Base
  ( FieldName (..),
    GQLError (..),
    GQLErrors,
    Message,
    TypeName (..),
    TypeRef,
    msg,
  )
import Data.Morpheus.Types.Internal.Validation.SchemaValidator
  ( FieldArg (..),
    Interface (..),
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
          <> renderField interfaceName fieldname
          <> detailedMessage errorType
      detailedMessage UnexpectedType {expectedType, foundType} =
        " expects type "
          <> msg expectedType
          <> " but "
          <> renderField typename fieldname
          <> " is type "
          <> msg foundType
          <> "."
      detailedMessage Missing =
        " expected but "
          <> msg typename
          <> " does not provide it."

-- Interface field TestInterface.name expected but User does not provide it.
-- Interface field TestInterface.name expects type String! but User.name is type Int!.

instance PartialImplements (Interface, FieldArg) where
  partialImplements (Interface interfaceName typename, FieldArg fieldname argName) errorType =
    [ GQLError
        { message = message,
          locations = []
        }
    ]
    where
      --
      message =
        "Interface field argument "
          <> renderArg interfaceName fieldname argName
          <> detailedMessage errorType
      detailedMessage UnexpectedType {expectedType, foundType} =
        "expects type"
          <> msg expectedType
          <> " but "
          <> renderArg typename fieldname argName
          <> "is type "
          <> msg foundType
          <> "."
      detailedMessage Missing =
        "expected but "
          <> renderField typename fieldname
          <> " does not provide it."

renderField :: TypeName -> FieldName -> Message
renderField (TypeName tname) (FieldName fname) =
  msg $ tname <> "." <> fname

renderArg :: TypeName -> FieldName -> FieldName -> Message
renderArg tname fname (FieldName argName) =
  renderField tname fname
    <> msg
      ( "("
          <> argName
          <> ":)"
          <> " "
      )

-- Interface field argument TestInterface.name(id:) expected but User.name does not provide it.
-- Interface field argument TestInterface.name(id:) expects type ID but User.name(id:) is type String.

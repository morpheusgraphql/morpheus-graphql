{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Document.Interface
  ( unknownInterface,
    partialImplements,
    ImplementsError (..),
    Place (..),
  )
where

import Data.Morpheus.Error.Utils (globalErrorMessage)
import Data.Morpheus.Types.Internal.AST.Base
  ( FieldName,
    GQLError (..),
    GQLErrors,
    TypeName,
    TypeRef,
    msg,
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

instance PartialImplements (TypeName, TypeName, FieldName) where
  partialImplements (typename, interfaceName, fieldname) errorType =
    [ GQLError
        { message = message,
          locations = []
        }
    ]
    where
      message =
        "type "
          <> msg typename
          <> " implements Interface "
          <> msg interfaceName
          <> " Partially,"
          <> detailedMessage errorType
      detailedMessage UnexpectedType {expectedType, foundType} =
        " on field "
          <> msg fieldname
          <> " expected type "
          <> msg expectedType
          <> " found "
          <> msg foundType
          <> "."
      detailedMessage Missing = " field " <> msg fieldname <> " not found ."

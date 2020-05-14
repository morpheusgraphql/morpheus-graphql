{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Document.Interface
  ( unknownInterface,
    partialImplements,
    ImplementsError (..),
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
  | UndefinedField

partialImplements :: TypeName -> [(TypeName, FieldName, ImplementsError)] -> GQLErrors
partialImplements name = map impError
  where
    impError (interfaceName, key, errorType) =
      GQLError
        { message = message,
          locations = []
        }
      where
        message =
          "type "
            <> msg name
            <> " implements Interface "
            <> msg interfaceName
            <> " Partially,"
            <> detailedMessage errorType
        detailedMessage UnexpectedType {expectedType, foundType} =
          " on key "
            <> msg key
            <> " expected type "
            <> msg expectedType
            <> " found "
            <> msg foundType
            <> "."
        detailedMessage UndefinedField = " key " <> msg key <> " not found ."

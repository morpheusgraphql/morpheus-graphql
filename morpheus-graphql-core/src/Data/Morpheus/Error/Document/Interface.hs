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
    Name,
    TypeName,
    msg,
  )
import Data.Semigroup ((<>))

unknownInterface :: Name -> GQLErrors
unknownInterface name = globalErrorMessage message
  where
    message = "Unknown Interface " <> msg name <> "."

data ImplementsError
  = UnexpectedType
      { expectedType :: TypeName,
        foundType :: TypeName
      }
  | UndefinedField

partialImplements :: TypeName -> [(Name, FieldName, ImplementsError)] -> GQLErrors
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

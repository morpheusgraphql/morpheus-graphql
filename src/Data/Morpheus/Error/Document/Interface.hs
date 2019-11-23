{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Document.Interface
  ( unknownInterface
  , partialImplements
  , ImplementsError(..)
  )
where

import           Data.Morpheus.Error.Utils      ( globalErrorMessage )
import           Data.Morpheus.Types.Internal.AST.Base
                                                ( Key )
import           Data.Morpheus.Types.Internal.Resolving.Core
                                                ( GQLError(..)
                                                , GQLErrors
                                                )
import           Data.Semigroup                 ( (<>) )

unknownInterface :: Key -> GQLErrors
unknownInterface name = globalErrorMessage message
  where message = "Unknown Interface \"" <> name <> "\"."

data ImplementsError
  = UnexpectedType { expectedType :: Key
                   , foundType    :: Key }
  | UndefinedField

partialImplements :: Key -> [(Key, Key, ImplementsError)] -> GQLErrors
partialImplements name = map impError
 where
  impError (interfaceName, key, errorType) = GQLError { message   = message
                                                      , locations = []
                                                      }
   where
    message =
      "type \""
        <> name
        <> "\" implements Interface \""
        <> interfaceName
        <> "\" Partially,"
        <> detailedMessage errorType
    detailedMessage UnexpectedType { expectedType, foundType } =
      " on key \""
        <> key
        <> "\" expected type \""
        <> expectedType
        <> "\" found \""
        <> foundType
        <> "\"."
    detailedMessage UndefinedField = " key \"" <> key <> "\" not found ."

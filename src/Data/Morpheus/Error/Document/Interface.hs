{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Document.Interface
  ( unknownInterface
  , partialImplements
  , ImplementsError(..)
  ) where

import           Data.Morpheus.Error.Utils               (errorMessage)
import           Data.Morpheus.Types.Internal.Base       (Key, Position)
import           Data.Morpheus.Types.Internal.Validation (GQLError (..), GQLErrors)
import           Data.Semigroup                          ((<>))

unknownInterface :: Key -> Position -> GQLErrors
unknownInterface name = (`errorMessage` message)
  where
    message = "Unknown Interface \"" <> name <> "\"."

data ImplementsError
  = UnexpectedType { expectedType :: Key
                   , foundType    :: Key }
  | UndefinedField

partialImplements :: Key -> Position -> [(Key, Key, ImplementsError)] -> GQLErrors
partialImplements name position = map impError
  where
    impError (interfaceName, key, errorType) = GQLError {desc = message, positions = [position]}
      where
        message =
          "type \"" <> name <> "\" implements Interface \"" <> interfaceName <> "\" Partially," <>
          detailedMessage errorType
        detailedMessage UnexpectedType {expectedType, foundType} =
          " on key \"" <> key <> "\" expected type \"" <> expectedType <> "\" found \"" <> foundType <> "\"."
        detailedMessage UndefinedField = " key \"" <> key <> "\" not found ."

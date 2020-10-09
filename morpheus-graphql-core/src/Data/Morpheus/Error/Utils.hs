{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Utils
  ( errorMessage,
    globalErrorMessage,
    badRequestError,
    validationErrorMessage,
  )
where

import Data.ByteString.Lazy.Char8
  ( ByteString,
    pack,
  )
import Data.Morpheus.Types.Internal.AST.Base
  ( GQLError (..),
    GQLErrors,
    Message,
    Position (..),
    ValidationError (..),
  )
import Relude hiding (ByteString)

validationErrorMessage :: Maybe Position -> Message -> ValidationError
validationErrorMessage pos message = ValidationError message (maybeToList pos)

errorMessage :: Position -> Message -> GQLErrors
errorMessage position message = [GQLError {message, locations = [position]}]

globalErrorMessage :: Message -> GQLErrors
globalErrorMessage message = [GQLError {message, locations = []}]

badRequestError :: String -> ByteString
badRequestError = ("Bad Request. Could not decode Request body: " <>) . pack

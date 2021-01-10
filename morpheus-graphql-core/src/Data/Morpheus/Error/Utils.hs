{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Error.Utils
  ( errorMessage,
    globalErrorMessage,
    validationErrorMessage,
  )
where

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

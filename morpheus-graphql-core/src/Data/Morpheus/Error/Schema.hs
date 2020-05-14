{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Schema
  ( nameCollisionError,
    schemaValidationError,
  )
where

import Data.Morpheus.Error.Utils (globalErrorMessage)
import Data.Morpheus.Types.Internal.AST.Base
  ( GQLErrors,
    Message,
    TypeName,
    msg,
  )
import Data.Semigroup ((<>))

schemaValidationError :: Message -> GQLErrors
schemaValidationError error' =
  globalErrorMessage $ "Schema Validation Error, " <> error'

nameCollisionError :: TypeName -> GQLErrors
nameCollisionError typeName =
  schemaValidationError $
    "Name collision: "
      <> msg typeName
      <> " is used for different dataTypes in two separate modules"

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Error.Schema
  ( nameCollisionError,
  )
where

import Data.Morpheus.Types.Internal.AST.Base
  ( TypeName,
    ValidationError,
    msgValidation,
  )
import Data.Semigroup ((<>))
import Prelude (($))

schemaValidationError :: ValidationError -> ValidationError
schemaValidationError error = "Schema Validation Error, " <> error

nameCollisionError :: TypeName -> ValidationError
nameCollisionError typeName =
  schemaValidationError $
    "Name collision: "
      <> msgValidation typeName
      <> " is used for different dataTypes in two separate modules"

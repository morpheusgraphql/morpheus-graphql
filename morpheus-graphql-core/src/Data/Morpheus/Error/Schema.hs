{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Schema
  ( nameCollisionError
  , schemaValidationError
  )
where

import           Data.Morpheus.Error.Utils      ( globalErrorMessage )
import           Data.Morpheus.Types.Internal.AST.Base
                                                ( GQLErrors )
import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text )

schemaValidationError :: Text -> GQLErrors
schemaValidationError error' =
  globalErrorMessage $ "Schema Validation Error, " <> error'

nameCollisionError :: Text -> GQLErrors
nameCollisionError name =
  schemaValidationError
    $  "Name collision: \""
    <> name
    <> "\" is used for different dataTypes in two separate modules"

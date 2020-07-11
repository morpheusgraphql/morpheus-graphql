{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Error.Internal
  ( typeMismatch,
    internalError,
    internalResolvingError,
  )
where

-- MORPHEUS
import Data.Morpheus.Error.Utils (globalErrorMessage)
import Data.Morpheus.Types.Internal.AST.Base
  ( GQLErrors,
    InternalError,
    Message,
    toInternalError,
  )
import Data.Morpheus.Types.Internal.AST.Value
  ( Value,
  )
import Data.Morpheus.Types.Internal.Resolving.Core
  ( Eventless,
    Failure (..),
  )
import Data.Semigroup ((<>))
import Prelude (($), (.))

-- GQL:: if no mutation defined -> "Schema is not configured for mutations."
-- all kind internal error in development
internalError :: Message -> Eventless a
internalError x = failure $ globalErrorMessage $ "INTERNAL ERROR: " <> x

internalResolvingError :: Message -> GQLErrors
internalResolvingError = globalErrorMessage . ("INTERNAL ERROR:" <>)

-- if value is already validated but value has different type
typeMismatch :: Message -> Value s -> InternalError
typeMismatch text jsType =
  "Type mismatch! expected:" <> toInternalError text <> ", got: "
    <> toInternalError jsType

{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Internal
  ( internalTypeMismatch,
    internalError,
    internalResolvingError,
  )
where

-- MORPHEUS
import Data.Morpheus.Error.Utils (globalErrorMessage)
import Data.Morpheus.Types.Internal.AST.Base
  ( GQLErrors,
    Message,
    msg,
  )
import Data.Morpheus.Types.Internal.AST.Value
  ( ValidValue,
  )
import Data.Morpheus.Types.Internal.Resolving.Core
  ( Eventless,
    Failure (..),
  )
import Data.Semigroup ((<>))

-- GQL:: if no mutation defined -> "Schema is not configured for mutations."
-- all kind internal error in development
internalError :: Message -> Eventless a
internalError x = failure $ globalErrorMessage $ "INTERNAL ERROR: " <> x

internalResolvingError :: Message -> GQLErrors
internalResolvingError = globalErrorMessage . ("INTERNAL ERROR:" <>)

-- if value is already validated but value has different type
internalTypeMismatch :: Message -> ValidValue -> Eventless a
internalTypeMismatch text jsType =
  internalError $ "Type mismatch " <> text <> msg jsType

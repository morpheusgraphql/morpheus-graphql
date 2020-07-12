{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Error.Internal
  ( typeMismatch,
    internalError,
  )
where

-- MORPHEUS
import Data.Morpheus.Error.Utils (globalErrorMessage)
import Data.Morpheus.Types.Internal.AST.Base
  ( GQLErrors,
    InternalError,
    Message,
    msgInteral,
  )
import Data.Morpheus.Types.Internal.AST.Value
  ( Value,
  )
import Data.Morpheus.Types.Internal.Resolving.Core
  ( Failure (..),
  )
import Data.Semigroup ((<>))
import Prelude (($))

-- GQL:: if no mutation defined -> "Schema is not configured for mutations."
-- all kind internal error in development
internalError :: Failure GQLErrors m => Message -> m a
internalError x = failure $ globalErrorMessage $ "INTERNAL ERROR: " <> x

-- if value is already validated but value has different type
typeMismatch :: Message -> Value s -> InternalError
typeMismatch text jsType =
  "Type mismatch! expected:" <> msgInteral text <> ", got: "
    <> msgInteral jsType

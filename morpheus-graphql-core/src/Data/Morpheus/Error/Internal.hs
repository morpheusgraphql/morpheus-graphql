{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Internal
  ( internalTypeMismatch
  , internalError
  , internalResolvingError
  )
where

import           Data.Text                      ( pack )
import           Data.Semigroup                 ( (<>) )

-- MORPHEUS
import           Data.Morpheus.Error.Utils      ( globalErrorMessage )
import           Data.Morpheus.Types.Internal.Resolving.Core
                                                ( Eventless
                                                , Failure(..)
                                                )
import           Data.Morpheus.Types.Internal.AST.Base
                                                ( GQLErrors 
                                                , Message
                                                )
import           Data.Morpheus.Types.Internal.AST.Value
                                                ( ValidValue )

-- GQL:: if no mutation defined -> "Schema is not configured for mutations."
-- all kind internal error in development
internalError :: Message -> Eventless a
internalError x = failure $ globalErrorMessage $ "INTERNAL ERROR: " <> x

internalResolvingError :: Message -> GQLErrors
internalResolvingError = globalErrorMessage . ("INTERNAL ERROR:" <>)

-- if value is already validated but value has different type
internalTypeMismatch :: Message -> ValidValue -> Eventless a
internalTypeMismatch text jsType =
  internalError $ "Type mismatch " <> text <> pack (show jsType)

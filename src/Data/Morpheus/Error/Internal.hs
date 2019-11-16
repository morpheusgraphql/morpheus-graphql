{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Internal
  ( internalTypeMismatch
  , internalArgumentError
  , internalUnknownTypeMessage
  , internalError
  )
where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
                                                ( concat
                                                , pack
                                                )
import           Data.Semigroup                 ( (<>) )

-- MORPHEUS
import           Data.Morpheus.Error.Utils      ( globalErrorMessage )
import           Data.Morpheus.Types.Internal.Validation
                                                ( Validation
                                                , GQLErrors
                                                , Failure(..)
                                                )
import           Data.Morpheus.Types.Internal.AST.Value
                                                ( Value(..) )


-- GQL:: if no mutation defined -> "Schema is not configured for mutations."
-- all kind internal error in development
internalError :: Text -> Validation b
internalError x = failure $ globalErrorMessage $ "INTERNAL ERROR: " <> x

-- if type did not not found, but was defined by Schema
internalUnknownTypeMessage :: Text -> GQLErrors
internalUnknownTypeMessage x = globalErrorMessage
  $ T.concat ["type did not not found, but was defined by Schema", x]

-- if arguments is already validated but has not found required argument
internalArgumentError :: Text -> Validation b
internalArgumentError x = internalError $ "Argument " <> x

-- if value is already validated but value has different type
internalTypeMismatch :: Text -> Value -> Validation b
internalTypeMismatch text jsType =
  internalError $ "Type mismatch " <> text <> T.pack (show jsType)

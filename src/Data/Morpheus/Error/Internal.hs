{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Internal
  ( internalTypeMismatch
  , internalArgumentError
  , internalUnknownTypeMessage
  , internalError
  , internalErrorIO
  ) where

import           Data.Morpheus.Error.Utils               (globalErrorMessage)
import           Data.Morpheus.Types.Internal.Validation (GQLErrors, ResolveIO, failResolveIO)
import           Data.Morpheus.Types.Internal.Value      (Value (..))
import           Data.Text                               (Text)
import qualified Data.Text                               as T (concat, pack)

-- GQL:: if no mutation defined -> "Schema is not configured for mutations."
-- all kind internal error in development
internalError :: Text -> Either GQLErrors b
internalError x = Left $ globalErrorMessage $ T.concat ["INTERNAL ERROR: ", x]

internalErrorIO :: Text -> ResolveIO b
internalErrorIO x = failResolveIO $ globalErrorMessage $ T.concat ["INTERNAL ERROR: ", x]

-- if type did not not found, but was defined by Schema
internalUnknownTypeMessage :: Text -> GQLErrors
internalUnknownTypeMessage x = globalErrorMessage $ T.concat ["type did not not found, but was defined by Schema", x]

-- if arguments is already validated but has not found required argument
internalArgumentError :: Text -> Either GQLErrors b
internalArgumentError x = internalError $ T.concat ["Argument ", x]

-- if value is already validated but value has different type
internalTypeMismatch :: Text -> Value -> Either GQLErrors b
internalTypeMismatch text jsType = internalError $ T.concat ["Type mismatch ", text, T.pack $ show jsType]

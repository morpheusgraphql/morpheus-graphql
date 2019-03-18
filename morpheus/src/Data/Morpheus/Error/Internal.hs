{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Internal
  ( internalError
  , internalTypeMismatch
  , internalUnresolvedField
  , internalArgumentError
  , internalUndefinedResolver
  ) where

import           Data.Morpheus.Error.Utils  (errorMessage)
import           Data.Morpheus.Types.Error  (GQLErrors)
import           Data.Morpheus.Types.JSType (JSType (..))
import qualified Data.Text                  as T (Text, concat, pack)

-- GQL:: if no mutation defined -> "Schema is not configured for mutations."
internalError :: T.Text -> Either GQLErrors b
internalError x = Left $ errorMessage 0 $ T.concat ["INTERNAL ERROR: ", x]

internalArgumentError :: T.Text -> Either GQLErrors b
internalArgumentError x = internalError $ T.concat ["Field Argument Error: ", x]

internalTypeMismatch :: T.Text -> JSType -> Either GQLErrors b
internalTypeMismatch text jsType = internalError $ T.concat ["Type mismatch", text, T.pack $ show jsType]

internalUnresolvedField :: T.Text -> Either GQLErrors b
internalUnresolvedField text = internalError $ T.concat ["Unresolverd field", text]

internalUndefinedResolver :: T.Text -> Either GQLErrors b
internalUndefinedResolver text = internalError $ T.concat ["Resolver not implemented Error: ", text]

{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Internal
  ( handleError
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
-- TODO: remove it
handleError :: T.Text -> Either GQLErrors b
handleError x = Left $ errorMessage 0 $ T.concat ["Field Error: ", x]

internalArgumentError :: T.Text -> Either GQLErrors b
internalArgumentError x = Left $ errorMessage 0 $ T.concat ["Field Argument Error: ", x]

internalTypeMismatch :: T.Text -> JSType -> Either GQLErrors b
internalTypeMismatch text jsType =
  Left $ errorMessage 0 $ T.concat ["Field Error: Type mismatch", text, T.pack $ show jsType]

internalUnresolvedField :: T.Text -> JSType -> Either GQLErrors b
internalUnresolvedField text jsType =
  Left $ errorMessage 0 $ T.concat ["Field Error: Unresolverd field", text, T.pack $ show jsType]

internalUndefinedResolver :: T.Text -> Either GQLErrors b
internalUndefinedResolver text = Left $ errorMessage 0 $ T.concat ["Field resolver not implemented Error: ", text]

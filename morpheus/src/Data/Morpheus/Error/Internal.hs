{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Internal
  ( internalTypeMismatch
  , internalUnresolvedField
  , internalArgumentError
  ) where

import           Data.Morpheus.Error.Utils  (errorMessage)
import           Data.Morpheus.Types.Error  (GQLErrors)
import           Data.Morpheus.Types.JSType (JSType (..))
import qualified Data.Text                  as T (Text, concat, pack)

-- GQL:: if no mutation defined -> "Schema is not configured for mutations."
-- all kind internal error in development
internalError :: T.Text -> Either GQLErrors b
internalError x = Left $ errorMessage 0 $ T.concat ["INTERNAL ERROR: ", x]

-- if arguments is already validated but has not found required argument
internalArgumentError :: T.Text -> Either GQLErrors b
internalArgumentError x = internalError $ T.concat ["Field Argument Error: ", x]

-- if value is already validated but value has different type
internalTypeMismatch :: T.Text -> JSType -> Either GQLErrors b
internalTypeMismatch text jsType = internalError $ T.concat ["Type mismatch", text, T.pack $ show jsType]

-- if field  a ::-> b resolver on selection has not selected
-- TODO: create new Resolver (a ::=> b) with pure function inside to remove this error
internalUnresolvedField :: T.Text -> Either GQLErrors b
internalUnresolvedField text = internalError $ T.concat ["Unresolverd field", text]

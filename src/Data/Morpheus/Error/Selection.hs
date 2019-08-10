{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Selection
  ( cannotQueryField
  , subfieldsNotSelected
  , duplicateQuerySelections
  , hasNoSubfields
  , fieldNotResolved
  ) where

import           Data.Morpheus.Error.Utils               (errorMessage)
import           Data.Morpheus.Types.Internal.Base       (EnhancedKey (..), Position)
import           Data.Morpheus.Types.Internal.Validation (GQLError (..), GQLErrors)
import           Data.Text                               (Text)
import qualified Data.Text                               as T (concat)

fieldNotResolved :: Position -> Text -> Text -> GQLErrors
fieldNotResolved position' key' message' = errorMessage position' text
  where
    text = T.concat ["Failure on Resolving Field \"", key', "\": ", message']

-- GQL: "Field \"default\" must not have a selection since type \"String!\" has no subfields."
hasNoSubfields :: Text -> Text -> Position -> GQLErrors
hasNoSubfields key typeName position = errorMessage position text
  where
    text = T.concat ["Field \"", key, "\" must not have a selection since type \"", typeName, "\" has no subfields."]

cannotQueryField :: Text -> Text -> Position -> GQLErrors
cannotQueryField key typeName position = errorMessage position text
  where
    text = T.concat ["Cannot query field \"", key, "\" on type \"", typeName, "\"."]

duplicateQuerySelections :: Text -> [EnhancedKey] -> GQLErrors
duplicateQuerySelections parentType = map keyToError
  where
    keyToError (EnhancedKey key' pos) = GQLError {desc = toMessage key', positions = [pos]}
    toMessage key' = T.concat ["duplicate selection of key \"", key', "\" on type \"", parentType, "\"."]

-- GQL:: Field \"hobby\" of type \"Hobby!\" must have a selection of subfields. Did you mean \"hobby { ... }\"?
subfieldsNotSelected :: Text -> Text -> Position -> GQLErrors
subfieldsNotSelected key typeName position = errorMessage position text
  where
    text = T.concat ["Field \"", key, "\" of type \"", typeName, "\" must have a selection of subfields"]

{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.ErrorMessage
  ( syntaxError
  , cannotQueryField
  , subfieldsNotSelected
  , handleError
  , errorMessage
  , invalidEnumOption
  , fieldTypeMismatch
  ) where

import           Data.Morpheus.Error.Utils    (errorMessage)
import           Data.Morpheus.Types.Error    (GQLErrors)
import           Data.Morpheus.Types.JSType   (JSType (..))
import           Data.Morpheus.Types.MetaInfo (MetaInfo (..), Position)
import           Data.Text                    (Text, pack)
import qualified Data.Text                    as T (concat)

-- GQL:: if no mutation defined -> "Schema is not configured for mutations."

handleError :: Text -> Either GQLErrors b
handleError x = Left $ errorMessage 0 $ T.concat ["Field Error: ", x]

invalidEnumOption :: MetaInfo -> GQLErrors
invalidEnumOption meta = errorMessage (position meta) text
  where
    text = T.concat ["Expected type ", typeName meta, " found ", key meta, "."]


fieldTypeMismatch :: MetaInfo -> JSType -> Text -> GQLErrors
fieldTypeMismatch meta isType should = errorMessage (position meta) text
  where
    text =
      T.concat
        [ "field \""
        , key meta
        , "\"on type \""
        , typeName meta
        , "\" has a type \""
        , pack $ show isType
        , "\" but should have \""
        , should
        , "\"."
        ]

-- GQL: "Field \"default\" must not have a selection since type \"String!\" has no subfields."

cannotQueryField :: MetaInfo -> GQLErrors
cannotQueryField meta = errorMessage (position meta) text
  where
    text = T.concat ["Cannot query field \"", key meta, "\" on type \"", typeName meta, "\"."]

-- GQL:: Field \"hobby\" of type \"Hobby!\" must have a selection of subfields. Did you mean \"hobby { ... }\"?
subfieldsNotSelected :: MetaInfo -> GQLErrors
subfieldsNotSelected meta = errorMessage (position meta) text
  where
    text = T.concat ["Field \"", key meta, "\" of type \"", typeName meta, "\" must have a selection of subfields"]

syntaxError :: Text -> Position -> GQLErrors
syntaxError e pos = errorMessage pos $ T.concat ["Syntax Error: ", e]

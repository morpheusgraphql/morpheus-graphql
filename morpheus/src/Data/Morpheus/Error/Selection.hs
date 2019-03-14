{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Selection
  ( cannotQueryField
  , subfieldsNotSelected
  ) where

import           Data.Morpheus.Error.Utils    (errorMessage)
import           Data.Morpheus.Types.Error    (GQLErrors)
import           Data.Morpheus.Types.MetaInfo (MetaInfo (..))
import qualified Data.Text                    as T (concat)

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

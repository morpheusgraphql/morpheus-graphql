{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Selection
  ( cannotQueryField
  , subfieldsNotSelected
  , hasNoSubfields
  )
where

import           Data.Semigroup                 ( (<>) )
import           Data.Morpheus.Error.Utils      ( errorMessage )
import           Data.Morpheus.Types.Internal.AST.Base
                                                ( Position
                                                , GQLErrors
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
                                                ( concat )


-- GQL: "Field \"default\" must not have a selection since type \"String!\" has no subfields."
hasNoSubfields :: Text -> Text -> Position -> GQLErrors
hasNoSubfields key typeName position = errorMessage position text
 where
  text = "Field \"" <> key <> "\" must not have a selection since type \"" <> typeName <> "\" has no subfields."

cannotQueryField :: Text -> Text -> Position -> GQLErrors
cannotQueryField key typeName position = errorMessage position text
 where
  text = "Cannot query field \"" <> key <> "\" on type \"" <> typeName <> "\"."

-- GQL:: Field \"hobby\" of type \"Hobby!\" must have a selection of subfields. Did you mean \"hobby { ... }\"?
subfieldsNotSelected :: Text -> Text -> Position -> GQLErrors
subfieldsNotSelected key typeName position = errorMessage position text
 where
  text = T.concat
    [ "Field \""
    , key
    , "\" of type \""
    , typeName
    , "\" must have a selection of subfields"
    ]

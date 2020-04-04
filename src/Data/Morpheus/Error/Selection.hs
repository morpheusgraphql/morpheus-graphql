{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Data.Morpheus.Error.Selection
  ( unknownSelectionField
  , subfieldsNotSelected
  , hasNoSubfields
  )
where

import           Data.Semigroup                 ( (<>) )
import           Data.Morpheus.Error.Utils      ( errorMessage )
import           Data.Morpheus.Types.Internal.AST.Base
                                                ( Position
                                                , GQLErrors
                                                , Ref(..)
                                                , Name
                                                )
import           Data.Text                      ( Text )

-- GQL: "Field \"default\" must not have a selection since type \"String!\" has no subfields."
hasNoSubfields :: Ref -> Name -> GQLErrors
hasNoSubfields (Ref selectionName position) typeName  = errorMessage position text
 where
  text = "Field \"" <> selectionName <> "\" must not have a selection since type \"" <> typeName <> "\" has no subfields."

unknownSelectionField :: Name -> Ref -> GQLErrors
unknownSelectionField typeName Ref { refName , refPosition } = errorMessage refPosition text
 where
  text = "Cannot query field \"" <> refName <> "\" on type \"" <> typeName <> "\"."

-- GQL:: Field \"hobby\" of type \"Hobby!\" must have a selection of subfields. Did you mean \"hobby { ... }\"?
subfieldsNotSelected :: Text -> Text -> Position -> GQLErrors
subfieldsNotSelected key typeName position = errorMessage position text
 where
  text = "Field \"" <> key <> "\" of type \""
    <> typeName <> "\" must have a selection of subfields"

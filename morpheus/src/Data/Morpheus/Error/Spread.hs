{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Spread
  ( unknownFragment
  , cannotBeSpreadOnType
  ) where

import           Data.Morpheus.Error.Utils    (errorMessage)
import           Data.Morpheus.Types.Error    (GQLErrors)
import           Data.Morpheus.Types.MetaInfo (Position)
import           Data.Text                    (Text)
import qualified Data.Text                    as T

-- {...H} -> "Unknown fragment \"H\"."
unknownFragment :: Text -> Position -> GQLErrors
unknownFragment key' position' = errorMessage position' text
  where
    text = T.concat ["Unknown Fragment \"", key', "\"."]

-- Fragment type mismatch -> "Fragment \"H\" cannot be spread here as objects of type \"Hobby\" can never be of type \"Experience\"."
cannotBeSpreadOnType :: Text -> Text -> Position -> Text -> GQLErrors
cannotBeSpreadOnType key' type' position' selectionType = errorMessage position' text
  where
    text =
      T.concat
        [ "Fragment \""
        , key'
        , "\" cannot be spread here as objects of type \""
        , type'
        , "\" can never be of type \""
        , selectionType
        , "\"."
        ]

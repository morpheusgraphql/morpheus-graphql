{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Spread
  ( unknownFragment
  , cannotBeSpreadOnType
  ) where

import           Data.Morpheus.Error.Utils    (errorMessage)
import           Data.Morpheus.Types.Error    (GQLErrors)
import           Data.Morpheus.Types.MetaInfo (MetaInfo (..), Position)
import           Data.Text                    (Text)
import qualified Data.Text                    as T

-- {...H} -> "Unknown fragment \"H\"."
unknownFragment :: Text -> Position -> GQLErrors
unknownFragment key' position' = errorMessage position' text
  where
    text = T.concat ["Unknown Fragment \"", key', "\"."]

-- Fragment type mismatch -> "Fragment \"H\" cannot be spread here as objects of type \"Hobby\" can never be of type \"Experience\"."
cannotBeSpreadOnType :: MetaInfo -> T.Text -> GQLErrors
cannotBeSpreadOnType spread selectionType = errorMessage (position spread) text
  where
    text =
      T.concat
        [ "Fragment \""
        , key spread
        , "\" cannot be spread here as objects of type \""
        , typeName spread
        , "\" can never be of type \""
        , selectionType
        , "\"."
        ]
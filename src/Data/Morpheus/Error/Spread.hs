{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Spread
  ( unknownFragment
  , cannotBeSpreadOnType
  ) where

import           Data.Morpheus.Error.Utils               (errorMessage)
import           Data.Morpheus.Types.Internal.Base       (Position)
import           Data.Morpheus.Types.Internal.Validation (GQLErrors)
import           Data.Text                               (Text)
import qualified Data.Text                               as T

-- {...H} -> "Unknown fragment \"H\"."
unknownFragment :: Text -> Position -> GQLErrors
unknownFragment key' position' = errorMessage position' text
  where
    text = T.concat ["Unknown Fragment \"", key', "\"."]

-- Fragment type mismatch -> "Fragment \"H\" cannot be spread here as objects of type \"Hobby\" can never be of type \"Experience\"."
cannotBeSpreadOnType :: Maybe Text -> Text -> Position -> Text -> GQLErrors
cannotBeSpreadOnType key' type' position' selectionType' = errorMessage position' text
  where
    text =
      T.concat
        [ "Fragment"
        , getName key'
        , " cannot be spread here as objects of type \""
        , selectionType'
        , "\" can never be of type \""
        , type'
        , "\"."
        ]
    getName (Just x') = T.concat [" \"", x', "\""]
    getName Nothing   = ""

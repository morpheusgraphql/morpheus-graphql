{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Fragment
  ( cannotSpreadWithinItself
  , unusedFragment
  , unknownFragment
  , cannotBeSpreadOnType
  , fragmentNameCollision
  ) where

import           Data.Semigroup                          ((<>))
import           Data.Text                               (Text)
import qualified Data.Text                               as T

-- MORPHEUS
import           Data.Morpheus.Error.Utils               (errorMessage)
import           Data.Morpheus.Types.Internal.Base       (EnhancedKey (..), Position)
import           Data.Morpheus.Types.Internal.Validation (GQLError (..), GQLErrors)

{-
  FRAGMENT:
    type Experience {
        experience ( lang: LANGUAGE ) : String ,
        date: String
    }
    fragment type mismatch -> "Fragment \"H\" cannot be spread here as objects of type \"Hobby\" can never be of type \"Experience\"."
    fragment H on T1 { ...A} , fragment A on T { ...H } -> "Cannot spread fragment \"H\" within itself via A."
    fragment H on D {...}  ->  "Unknown type \"D\"."
    {...H} -> "Unknown fragment \"H\"."
-}
fragmentNameCollision :: [EnhancedKey] -> GQLErrors
fragmentNameCollision = map toError
  where
    toError EnhancedKey {uid, location} =
      GQLError {desc = "There can be only one fragment named \"" <> uid <> "\".", positions = [location]}

unusedFragment :: [EnhancedKey] -> GQLErrors
unusedFragment = map toError
  where
    toError EnhancedKey {uid, location} =
      GQLError {desc = "Fragment \"" <> uid <> "\" is never used.", positions = [location]}

cannotSpreadWithinItself :: [EnhancedKey] -> GQLErrors
cannotSpreadWithinItself fragments = [GQLError {desc = text, positions = map location fragments}]
  where
    text =
      T.concat
        [ "Cannot spread fragment \""
        , uid $ head fragments
        , "\" within itself via "
        , T.intercalate "," (map uid fragments)
        , "."
        ]

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

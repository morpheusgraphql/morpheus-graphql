{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Fragment
  ( cannotSpreadWithinItself
  , cannotBeSpreadOnType
  )
where

import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text
                                                , intercalate
                                                )
import qualified Data.Text                     as T

-- MORPHEUS
import           Data.Morpheus.Error.Utils      ( errorMessage )
import           Data.Morpheus.Types.Internal.AST.Base
                                                ( Ref(..)
                                                , Position
                                                , GQLError(..)
                                                , GQLErrors
                                                )

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

cannotSpreadWithinItself :: [Ref] -> GQLErrors
cannotSpreadWithinItself fragments = [GQLError { message = text, locations = map refPosition fragments }]
 where
  text = "Cannot spread fragment \""
    <> refName (head fragments)
    <> "\" within itself via "
    <> T.intercalate ", " (map refName fragments)
    <> "."

-- Fragment type mismatch -> "Fragment \"H\" cannot be spread here as objects of type \"Hobby\" can never be of type \"Experience\"."
cannotBeSpreadOnType :: Maybe Text -> Text -> Position -> [Text] -> GQLErrors
cannotBeSpreadOnType key fragmentType position typeMembers = errorMessage
  position
  msg
 where
  msg =
    "Fragment "
      <> getName key
      <> "cannot be spread here as objects of type \""
      <> intercalate ", " typeMembers
      <> "\" can never be of type \""
      <> fragmentType
      <> "\"."
  getName (Just x) = "\"" <> x <> "\" "
  getName Nothing  = ""

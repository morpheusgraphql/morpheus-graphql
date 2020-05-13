{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Fragment
  ( cannotSpreadWithinItself,
    cannotBeSpreadOnType,
  )
where

-- MORPHEUS
import Data.Morpheus.Error.Utils (errorMessage)
import Data.Morpheus.Types.Internal.AST.Base
  ( FieldName,
    GQLError (..),
    GQLErrors,
    Position,
    Ref (..),
    TypeName,
    msg,
  )
import Data.Semigroup ((<>))

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
cannotSpreadWithinItself fragments = [GQLError {message = text, locations = map refPosition fragments}]
  where
    text =
      "Cannot spread fragment "
        <> msg (refName $ head fragments)
        <> " within itself via "
        <> msg (map refName fragments)
        <> "."

-- Fragment type mismatch -> "Fragment \"H\" cannot be spread here as objects of type \"Hobby\" can never be of type \"Experience\"."
cannotBeSpreadOnType :: Maybe FieldName -> TypeName -> Position -> [TypeName] -> GQLErrors
cannotBeSpreadOnType key fragmentType position typeMembers =
  errorMessage
    position
    text
  where
    text =
      "Fragment "
        <> getName key
        <> "cannot be spread here as objects of type "
        <> msg typeMembers
        <> " can never be of type "
        <> msg fragmentType
        <> "."
    getName (Just x) = msg ("\"" <> x <> "\" ")
    getName Nothing = ""

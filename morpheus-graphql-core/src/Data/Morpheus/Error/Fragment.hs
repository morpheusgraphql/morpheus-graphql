{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Fragment
  ( cannotSpreadWithinItself,
    cannotBeSpreadOnType,
  )
where

-- MORPHEUS
import Data.Morpheus.Error.Utils (validationErrorMessage)
import Data.Morpheus.Types.Internal.AST.Base
  ( FieldName,
    Position,
    Ref (..),
    TypeName,
    ValidationError (..),
    msg,
    msgSepBy,
  )
import Relude

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

cannotSpreadWithinItself :: NonEmpty Ref -> ValidationError
cannotSpreadWithinItself (fr :| frs) = ValidationError text (fmap refPosition (fr : frs))
  where
    text =
      "Cannot spread fragment "
        <> msg (refName fr)
        <> " within itself via "
        <> msgSepBy ", " (fmap refName (fr : frs))
        <> "."

-- Fragment type mismatch -> "Fragment \"H\" cannot be spread here as objects of type \"Hobby\" can never be of type \"Experience\"."
cannotBeSpreadOnType :: Maybe FieldName -> TypeName -> Position -> [TypeName] -> ValidationError
cannotBeSpreadOnType key fragmentType position typeMembers =
  validationErrorMessage
    (Just position)
    text
  where
    text =
      "Fragment "
        <> getName key
        <> "cannot be spread here as objects of type "
        <> msgSepBy ", " typeMembers
        <> " can never be of type "
        <> msg fragmentType
        <> "."
    getName (Just x) = msg x <> " "
    getName Nothing = ""

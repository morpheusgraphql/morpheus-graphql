{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Error.Fragment
  ( cannotSpreadWithinItself,
    cannotBeSpreadOnType,
  )
where

-- MORPHEUS

import Data.Maybe (Maybe (..))
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
import Data.Semigroup ((<>))
import Prelude (($), fmap, head)

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

cannotSpreadWithinItself :: [Ref] -> ValidationError
cannotSpreadWithinItself fragments = ValidationError text (fmap refPosition fragments)
  where
    text =
      "Cannot spread fragment "
        <> msg (refName $ head fragments)
        <> " within itself via "
        <> msgSepBy ", " (fmap refName fragments)
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

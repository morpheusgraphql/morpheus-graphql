{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Error.Fragment
  ( cannotSpreadWithinItself,
    cannotBeSpreadOnType,
  )
where

-- MORPHEUS
import Data.Morpheus.Types.Internal.AST.Base
  ( Position,
    Ref (..),
  )
import Data.Morpheus.Types.Internal.AST.Error
  ( GQLError,
    at,
    atPositions,
    manyMsg,
    msg,
  )
import Data.Morpheus.Types.Internal.AST.Name
  ( FragmentName,
    TypeName,
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

cannotSpreadWithinItself :: NonEmpty (Ref FragmentName) -> GQLError
cannotSpreadWithinItself (fr :| frs) =
  ( "Cannot spread fragment "
      <> msg (refName fr)
      <> " within itself via "
      <> manyMsg (refName <$> (fr : frs))
      <> "."
  )
    `atPositions` map refPosition (fr : frs)

-- Fragment type mismatch -> "Fragment \"H\" cannot be spread here as objects of type \"Hobby\" can never be of type \"Experience\"."
cannotBeSpreadOnType :: Maybe FragmentName -> TypeName -> Position -> [TypeName] -> GQLError
cannotBeSpreadOnType key fragmentType position typeMembers =
  ( "Fragment "
      <> getName key
      <> "cannot be spread here as objects of type "
      <> manyMsg typeMembers
      <> " can never be of type "
      <> msg fragmentType
      <> "."
  )
    `at` position
  where
    getName (Just x) = msg x <> " "
    getName Nothing = ""

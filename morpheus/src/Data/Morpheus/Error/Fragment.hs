{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Fragment
  ( unknownFragment
  , unsupportedSpreadOnType
  , cycleOnFragment
  , fragmentError
  ) where

import           Data.Morpheus.Error.Utils    (errorMessage)
import           Data.Morpheus.Types.Error    (GQLErrors, MetaError (..))
import           Data.Morpheus.Types.MetaInfo (MetaInfo (..))
import qualified Data.Text                    as T

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

fragmentError :: MetaError -> GQLErrors
fragmentError (UnknownType meta) = unknownFragment meta

unknownFragment :: MetaInfo -> GQLErrors
unknownFragment meta = errorMessage (position meta) text
  where
    text = T.concat ["Unknown fragment \"", key meta, "\"."]

unsupportedSpreadOnType :: MetaInfo -> MetaInfo -> GQLErrors
unsupportedSpreadOnType parent spread = errorMessage (position parent) text
  where
    text =
      T.concat
        [ "cant apply fragment \""
        , key spread
        , "\" with type \""
        , typeName spread
        , "\" on type \""
        , typeName parent
        , "\"."
        ]

cycleOnFragment :: [T.Text] -> GQLErrors
cycleOnFragment fragments =
  errorMessage 0 $ T.concat ["fragment \"", head fragments, "\" has cycle \"", T.intercalate "," fragments, "\"."]

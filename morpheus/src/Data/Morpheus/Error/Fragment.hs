{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Fragment
  ( unknownFragment
  , unsupportedSpreadOnType
  , cycleOnFragment
  ) where

import           Data.Morpheus.Error.Utils    (errorMessage)
import           Data.Morpheus.Types.Error    (GQLError (..), GQLErrors)
import           Data.Morpheus.Types.MetaInfo (MetaInfo (..))
import qualified Data.Text                    as T

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

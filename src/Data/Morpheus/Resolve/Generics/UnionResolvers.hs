{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Morpheus.Resolve.Generics.UnionResolvers
  ( UnionResolvers(..)
  , lookupSelectionByType
  ) where

import           Data.Maybe                                 (fromMaybe)
import           Data.Morpheus.Types.Internal.AST.Selection (Selection, SelectionSet)
import           Data.Morpheus.Types.Internal.Validation    (ResolveIO)
import           Data.Text                                  (Text)
import           GHC.Generics

-- SPEC: if there is no any fragment that supports current object Type GQL returns {}
lookupSelectionByType :: Text -> [(Text, SelectionSet)] -> SelectionSet
lookupSelectionByType type' sel = fromMaybe [] $ lookup type' sel

class UnionResolvers f res where
  currentResolver :: f a -> (Text, (Text, Selection) -> ResolveIO res)

instance UnionResolvers f res => UnionResolvers (M1 S s f) res where
  currentResolver (M1 x) = currentResolver x

instance UnionResolvers f res => UnionResolvers (M1 D c f) res where
  currentResolver (M1 x) = currentResolver x

instance UnionResolvers f res => UnionResolvers (M1 C c f) res where
  currentResolver (M1 x) = currentResolver x

instance (UnionResolvers a res, UnionResolvers b res) => UnionResolvers (a :+: b) res where
  currentResolver (L1 x) = currentResolver x
  currentResolver (R1 x) = currentResolver x

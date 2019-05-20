{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Morpheus.Generics.UnionResolvers
  ( UnionResolvers(..)
  ) where

import           Data.Morpheus.Types.Error           (ResolveIO)
import           Data.Morpheus.Types.Internal.Value  (Value (..))
import           Data.Morpheus.Types.Query.Selection (Selection)
import           Data.Text                           (Text)
import           GHC.Generics

class UnionResolvers f where
  currentResolver :: f a -> (Text, (Text, Selection) -> ResolveIO Value)

instance UnionResolvers f => UnionResolvers (M1 S s f) where
  currentResolver (M1 x) = currentResolver x

instance UnionResolvers f => UnionResolvers (M1 D c f) where
  currentResolver (M1 x) = currentResolver x

instance UnionResolvers f => UnionResolvers (M1 C c f) where
  currentResolver (M1 x) = currentResolver x

instance (UnionResolvers a, UnionResolvers b) => UnionResolvers (a :+: b) where
  currentResolver (L1 x) = currentResolver x
  currentResolver (R1 x) = currentResolver x

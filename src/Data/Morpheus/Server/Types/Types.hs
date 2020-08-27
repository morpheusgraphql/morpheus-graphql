{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Types.Types
  ( Undefined (..),
    Pair (..),
    MapKind (..),
    mapKindFromList,
  )
where

import Data.Functor (fmap)
import GHC.Generics
  ( Generic,
  )
import Prelude
  ( Applicative (..),
    Int,
    Show,
    length,
    uncurry,
  )

data Undefined (m :: * -> *) = Undefined deriving (Show, Generic)

data Pair k v = Pair
  { key :: k,
    value :: v
  }
  deriving (Generic)

data MapKind k v m = MapKind
  { size :: Int,
    pairs :: m [Pair k v]
  }
  deriving (Generic)

mapKindFromList :: (Applicative m) => [(k, v)] -> MapKind k v m
mapKindFromList inputPairs =
  MapKind
    { size = length inputPairs,
      pairs = resolvePairs
    }
  where
    resolvePairs = pure (fmap (uncurry Pair) inputPairs)

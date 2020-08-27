{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}

module Data.Morpheus.Server.Types.Types
  ( Undefined (..),
    Pair (..),
    MapKind (..),
    mapKindFromList,
  )
where

import GHC.Generics (Generic)

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
    resolvePairs = pure (map (uncurry Pair) inputPairs)

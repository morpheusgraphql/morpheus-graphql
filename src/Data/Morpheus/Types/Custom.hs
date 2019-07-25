{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Morpheus.Types.Custom
  ( Pair(..)
  , MapKind(..)
  , MapArgs(..)
  , mapKindFromList
  ) where

import           GHC.Generics (Generic)

data Pair k v = Pair
  { key   :: k
  , value :: v
  } deriving (Generic)

newtype MapArgs k = MapArgs
  { oneOf :: Maybe [k]
  } deriving (Generic)

data MapKind k v m = MapKind
  { size  :: Int
  , pairs :: MapArgs k -> m [Pair k v]
  } deriving (Generic)

mapKindFromList :: (Eq k, Monad m) => [(k, v)] -> MapKind k v m
mapKindFromList inputPairs = MapKind {size = length inputPairs, pairs = resolvePairs}
  where
    filterBy MapArgs {oneOf = Just list} = filter ((`elem` list) . fst) inputPairs
    filterBy _                           = inputPairs
    resolvePairs = return . (map toGQLTuple . filterBy)
    toGQLTuple (x, y) = Pair x y

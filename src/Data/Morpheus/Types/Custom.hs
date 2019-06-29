{-# LANGUAGE DeriveAnyClass      #-}
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

import           GHC.Generics                (Generic)

-- MORPHEUS
import           Data.Morpheus.Kind          (KIND, OBJECT)
import           Data.Morpheus.Types.GQLType (GQLType)

type instance KIND (Pair k v) = OBJECT

data Pair k v = Pair
  { key   :: k
  , value :: v
  } deriving (Generic, GQLType)

newtype MapArgs k = MapArgs
  { oneOf :: Maybe [k]
  } deriving (Generic)

type instance KIND (MapKind k v m) = OBJECT

data MapKind k v m = MapKind
  { size   :: Int
  , keys   :: () -> m [k]
  , values :: MapArgs k -> m [v]
  , pairs  :: MapArgs k -> m [Pair k v]
  } deriving (Generic, GQLType)

mapKindFromList :: (Eq k, Monad m) => [(k, v)] -> MapKind k v m
mapKindFromList inputPairs =
  MapKind {size = length inputPairs, keys = resolveKeys, values = resolveValues, pairs = resolvePairs}
  where
    filterBy MapArgs {oneOf = Just list} = filter ((`elem` list) . fst) inputPairs
    filterBy _                           = inputPairs
    resolveKeys _ = return $ map fst inputPairs
    resolveValues = return . map snd . filterBy
    resolvePairs = return . (map toGQLTuple . filterBy)
    toGQLTuple (x, y) = Pair x y

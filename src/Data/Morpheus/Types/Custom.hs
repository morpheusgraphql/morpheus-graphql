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

import           Data.Morpheus.Kind           (KIND, OBJECT)
import           Data.Morpheus.Types.GQLType  (GQLType)
import           Data.Morpheus.Types.Resolver (QUERY, Resolver (..))
import           GHC.Generics                 (Generic)

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
  , keys   :: Resolver m (MapArgs k) [k]
  , values :: Resolver m (MapArgs k) [v]
  , pairs  :: Resolver m (MapArgs k) [Pair k v]
  } deriving (Generic, GQLType)

mapKindFromList :: (Eq k, Monad m) => [(k, v)] -> MapKind k v (QUERY m)
mapKindFromList inputPairs =
  MapKind
    { size = length inputPairs
    , keys = Resolver resolveKeys
    , values = Resolver resolveValues
    , pairs = Resolver resolvePairs
    }
  where
    filterBy MapArgs {oneOf = Just list} = filter ((`elem` list) . fst) inputPairs
    filterBy _                           = inputPairs
    resolveKeys = return . Right . map fst . filterBy
    resolveValues = return . Right . map snd . filterBy
    resolvePairs = return . Right . map toGQLTuple . filterBy
    toGQLTuple (x, y) = Pair x y

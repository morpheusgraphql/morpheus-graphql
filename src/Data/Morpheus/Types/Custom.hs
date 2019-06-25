{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Morpheus.Types.Custom
  ( Tuple(..)
  , MapKind(..)
  , MapArgs(..)
  , mapKindFromList
  ) where

import           Data.Morpheus.Kind           (KIND, OBJECT)
import           Data.Morpheus.Types.GQLType  (GQLType)
import           Data.Morpheus.Types.Resolver (QUERY, Resolver (..))
import           GHC.Generics                 (Generic)

type instance KIND (Tuple a b) = OBJECT

data Tuple a b = Tuple
  { key   :: a
  , value :: b
  } deriving (Generic, GQLType)

newtype MapArgs k = MapArgs
  { oneOf :: Maybe [k]
  } deriving (Generic)

type instance KIND (MapKind a b m) = OBJECT

data MapKind k v m = MapKind
  { size   :: Int
  , keys   :: Resolver m (MapArgs k) [k]
  , values :: Resolver m (MapArgs k) [v]
  , tuples :: Resolver m (MapArgs k) [Tuple k v]
  } deriving (Generic, GQLType)

mapKindFromList :: (Eq k, Monad m) => [(k, v)] -> MapKind k v (QUERY m)
mapKindFromList pairs =
  MapKind
    {size = length pairs, keys = Resolver resolveKeys, values = Resolver resolveValues, tuples = Resolver resolvePairs}
  where
    filterBy MapArgs {oneOf = Just list} = filter ((`elem` list) . fst) pairs
    filterBy _                           = pairs
    resolveKeys args = return $ Right $ map fst $ filterBy args
    resolveValues args = return $ Right $ map snd $ filterBy args
    resolvePairs args = return $ Right $ map toGQLTuple $ filterBy args
    toGQLTuple (x, y) = Tuple x y

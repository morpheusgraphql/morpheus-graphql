{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.AST.OrderedMap
  ( OrderedMap (..),
    unsafeFromValues,
  )
where

-- MORPHEUS

import Control.Applicative (Applicative (..))
import Data.Foldable (Foldable (..))
import Data.Functor ((<$>), Functor (..))
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe, isJust, maybe)
import Data.Morpheus.Error.NameCollision (NameCollision (..))
import Data.Morpheus.Internal.Utils
  ( Collection (..),
    Failure (..),
    KeyOf (..),
    Listable (..),
    Merge (..),
    Selectable (..),
    toPair,
  )
import Data.Morpheus.Types.Internal.AST.Base
  ( GQLErrors,
  )
import Data.Semigroup ((<>))
import Data.Traversable (Traversable (..))
import Language.Haskell.TH.Syntax (Lift (..))
import Prelude
  ( ($),
    (.),
    Eq,
    Show,
    error,
    otherwise,
    uncurry,
  )

-- OrderedMap
data OrderedMap k a = OrderedMap
  { mapKeys :: [k],
    mapEntries :: HashMap k a
  }
  deriving (Show, Eq, Functor)

instance (Lift a, Lift k) => Lift (OrderedMap k a) where
  lift (OrderedMap names x) = [|OrderedMap names (HM.fromList ls)|]
    where
      ls = HM.toList x

instance (Eq k, Hashable k) => Foldable (OrderedMap k) where
  foldMap f = foldMap f . getElements

getElements :: (Eq k, Hashable k) => OrderedMap k b -> [b]
getElements OrderedMap {mapKeys, mapEntries} = fmap takeValue mapKeys
  where
    takeValue key = fromMaybe (error "TODO: invalid Ordered Map") (key `HM.lookup` mapEntries)

instance (Eq k, Hashable k) => Traversable (OrderedMap k) where
  traverse f (OrderedMap names values) = OrderedMap names <$> traverse f values

instance (KeyOf a, Hashable k, KEY a ~ k) => Collection a (OrderedMap k a) where
  empty = OrderedMap [] HM.empty
  singleton x = OrderedMap [keyOf x] $ HM.singleton (keyOf x) x

instance (Eq k, Hashable k, k ~ KEY a) => Selectable (OrderedMap k a) a where
  selectOr fb f key OrderedMap {mapEntries} = maybe fb f (HM.lookup key mapEntries)

instance (NameCollision a, Eq k, Hashable k, k ~ KEY a) => Merge (OrderedMap k a) where
  merge _ (OrderedMap k1 x) (OrderedMap k2 y) = OrderedMap (k1 <> k2) <$> safeJoin x y

instance (NameCollision a, Eq k, Hashable k, k ~ KEY a) => Listable a (OrderedMap k a) where
  fromElems = safeFromList
  elems = getElements

safeFromList ::
  ( Failure GQLErrors m,
    Applicative m,
    NameCollision a,
    Eq (KEY a),
    Hashable (KEY a),
    KeyOf a
  ) =>
  [a] ->
  m (OrderedMap (KEY a) a)
safeFromList values = OrderedMap (fmap keyOf values) <$> safeUnionWith HM.empty (fmap toPair values)

unsafeFromValues ::
  ( KeyOf a,
    Eq (KEY a),
    Hashable (KEY a)
  ) =>
  [a] ->
  OrderedMap (KEY a) a
unsafeFromValues x = OrderedMap (fmap keyOf x) $ HM.fromList $ fmap toPair x

safeJoin :: (Failure GQLErrors m, Eq k, Hashable k, KEY a ~ k, Applicative m, NameCollision a) => HashMap k a -> HashMap k a -> m (HashMap k a)
safeJoin hm newls = safeUnionWith hm (HM.toList newls)

safeUnionWith ::
  ( Failure GQLErrors m,
    Applicative m,
    Eq k,
    Hashable k,
    NameCollision a,
    KEY a ~ k
  ) =>
  HashMap k a ->
  [(k, a)] ->
  m (HashMap k a)
safeUnionWith hm names = case insertNoDups (hm, []) names of
  (res, dupps)
    | null dupps -> pure res
    | otherwise -> failure $ fmap (uncurry nameCollision) dupps

type NoDupHashMap k a = (HashMap k a, [(k, a)])

insertNoDups :: (Eq k, Hashable k) => NoDupHashMap k a -> [(k, a)] -> NoDupHashMap k a
insertNoDups collected [] = collected
insertNoDups (coll, errors) (pair@(name, value) : xs)
  | isJust (name `HM.lookup` coll) = insertNoDups (coll, errors <> [pair]) xs
  | otherwise = insertNoDups (HM.insert name value coll, errors) xs

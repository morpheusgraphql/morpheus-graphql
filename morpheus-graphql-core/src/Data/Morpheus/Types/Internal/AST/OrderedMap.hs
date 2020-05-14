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

module Data.Morpheus.Types.Internal.AST.OrderedMap
  ( OrderedMap (..),
    unsafeFromValues,
    update,
  )
where

import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe, isJust)
-- MORPHEUS
import Data.Morpheus.Error.NameCollision (NameCollision (..))
import Data.Morpheus.Internal.Utils
  ( Empty (..),
    Failure (..),
    KeyOf (..),
    Listable (..),
    Merge (..),
    Selectable (..),
    Singleton (..),
    toPair,
  )
import Data.Morpheus.Types.Internal.AST.Base
  ( GQLErrors,
  )
import Data.Semigroup ((<>))
import Language.Haskell.TH.Syntax (Lift (..))

-- OrderedMap
data OrderedMap k a = OrderedMap
  { mapKeys :: [k],
    mapEntries :: HashMap k a
  }
  deriving (Show, Eq, Functor)

update :: (KeyOf a, KEY a ~ k, Eq k, Hashable k) => a -> OrderedMap k a -> OrderedMap k a
update x (OrderedMap names values) = OrderedMap newNames $ HM.insert name x values
  where
    name = keyOf x
    newNames
      | name `elem` names = names
      | otherwise = names <> [name]

instance (Lift a, Lift k) => Lift (OrderedMap k a) where
  lift (OrderedMap names x) = [|OrderedMap names (HM.fromList ls)|]
    where
      ls = HM.toList x

instance Foldable (OrderedMap k) where
  foldMap f OrderedMap {mapEntries} = foldMap f mapEntries

instance Traversable (OrderedMap k) where
  traverse f (OrderedMap names values) = OrderedMap names <$> traverse f values

instance Empty (OrderedMap k a) where
  empty = OrderedMap [] HM.empty

instance (KeyOf a, Hashable k, KEY a ~ k) => Singleton (OrderedMap k a) a where
  singleton x = OrderedMap [keyOf x] $ HM.singleton (keyOf x) x

instance (Eq k, Hashable k, k ~ KEY a) => Selectable (OrderedMap k a) a where
  selectOr fb f key OrderedMap {mapEntries} = maybe fb f (HM.lookup key mapEntries)

instance (NameCollision a, Eq k, Hashable k, k ~ KEY a) => Merge (OrderedMap k a) where
  merge _ (OrderedMap k1 x) (OrderedMap k2 y) = OrderedMap (k1 <> k2) <$> safeJoin x y

instance (NameCollision a, Eq k, Hashable k, k ~ KEY a) => Listable (OrderedMap k a) a where
  fromElems = safeFromList
  elems OrderedMap {mapKeys, mapEntries} = map takeValue mapKeys
    where
      takeValue key = fromMaybe (error "TODO: invalid Ordered Map") (key `HM.lookup` mapEntries)

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
safeFromList values = OrderedMap (map keyOf values) <$> safeUnionWith HM.empty (map toPair values)

unsafeFromValues ::
  ( KeyOf a,
    Eq (KEY a),
    Hashable (KEY a)
  ) =>
  [a] ->
  OrderedMap (KEY a) a
unsafeFromValues x = OrderedMap (map keyOf x) $ HM.fromList $ fmap toPair x

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
    | otherwise -> failure $ map (uncurry nameCollision) dupps

type NoDupHashMap k a = (HashMap k a, [(k, a)])

insertNoDups :: (Eq k, Hashable k) => NoDupHashMap k a -> [(k, a)] -> NoDupHashMap k a
insertNoDups collected [] = collected
insertNoDups (coll, errors) (pair@(name, value) : xs)
  | isJust (name `HM.lookup` coll) = insertNoDups (coll, errors <> [pair]) xs
  | otherwise = insertNoDups (HM.insert name value coll, errors) xs

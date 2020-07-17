{-# LANGUAGE CPP #-}
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

module Data.Morpheus.Types.Internal.AST.OrdMap
  ( OrdMap (..),
    unsafeFromValues,
    safeJoin,
    safeUnionWith,
    insertNoDups,
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
    snd,
  )

-- OrdMap
data OrdMap k a = OrdMap
  { mapKeys :: [k],
    mapEntries :: HashMap k a
  }
  deriving (Show, Eq, Functor)

instance (Lift a, Lift k, Eq k, Hashable k) => Lift (OrdMap k a) where
  lift (OrdMap names x) = [|OrdMap names (HM.fromList ls)|]
    where
      ls = HM.toList x

#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped (OrdMap names x) = [||OrdMap names (HM.fromList ls)||]
    where
      ls = HM.toList x
#endif

instance (Eq k, Hashable k) => Foldable (OrdMap k) where
  foldMap f = foldMap f . getElements

getElements :: (Eq k, Hashable k) => OrdMap k b -> [b]
getElements OrdMap {mapKeys, mapEntries} = fmap takeValue mapKeys
  where
    takeValue key = fromMaybe (error "TODO: invalid Ordered Map") (key `HM.lookup` mapEntries)

instance (Eq k, Hashable k) => Traversable (OrdMap k) where
  traverse f (OrdMap names values) = OrdMap names <$> traverse f values

instance (KeyOf k a, Hashable k) => Collection a (OrdMap k a) where
  empty = OrdMap [] HM.empty
  singleton x = OrdMap [keyOf x] $ HM.singleton (keyOf x) x

instance (Eq k, Hashable k) => Selectable k a (OrdMap k a) where
  selectOr fb f key OrdMap {mapEntries} = maybe fb f (HM.lookup key mapEntries)

instance (NameCollision a, Eq k, Hashable k) => Merge (OrdMap k a) where
  merge _ (OrdMap k1 x) (OrdMap k2 y) = OrdMap (k1 <> k2) <$> safeJoin x y

instance (NameCollision a, KeyOf k a, Hashable k) => Listable a (OrdMap k a) where
  fromElems = safeFromList
  elems = getElements

safeFromList ::
  ( Failure GQLErrors m,
    Applicative m,
    NameCollision a,
    Eq k,
    Hashable k,
    KeyOf k a
  ) =>
  [a] ->
  m (OrdMap k a)
safeFromList values = OrdMap (fmap keyOf values) <$> safeUnionWith HM.empty (fmap toPair values)

unsafeFromValues ::
  ( KeyOf k a,
    Hashable k
  ) =>
  [a] ->
  OrdMap k a
unsafeFromValues x = OrdMap (fmap keyOf x) $ HM.fromList $ fmap toPair x

safeJoin :: (Failure GQLErrors m, Eq k, Hashable k, Applicative m, NameCollision a) => HashMap k a -> HashMap k a -> m (HashMap k a)
safeJoin hm newls = safeUnionWith hm (HM.toList newls)

safeUnionWith ::
  ( Failure GQLErrors m,
    Applicative m,
    Eq k,
    Hashable k,
    NameCollision a
  ) =>
  HashMap k a ->
  [(k, a)] ->
  m (HashMap k a)
safeUnionWith hm names = case insertNoDups (hm, []) names of
  (res, dupps)
    | null dupps -> pure res
    | otherwise -> failure $ fmap (nameCollision . snd) dupps

type NoDupHashMap k a = (HashMap k a, [(k, a)])

insertNoDups :: (Eq k, Hashable k) => NoDupHashMap k a -> [(k, a)] -> NoDupHashMap k a
insertNoDups collected [] = collected
insertNoDups (coll, errors) (pair@(name, value) : xs)
  | isJust (name `HM.lookup` coll) = insertNoDups (coll, errors <> [pair]) xs
  | otherwise = insertNoDups (HM.insert name value coll, errors) xs

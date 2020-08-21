{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.AST.MergeSet
  ( MergeSet,
    toOrdMap,
    concatTraverse,
    join,
  )
where

-- MORPHEUS

import Control.Applicative (Applicative (..))
import Control.Monad (Monad (..))
import Data.Foldable (Foldable (..))
import Data.Functor (Functor (..))
import Data.List (find)
import Data.Maybe (maybe)
import Data.Morpheus.Internal.Utils
  ( (<:>),
    Collection (..),
    Failure (..),
    KeyOf (..),
    Listable (..),
    Merge (..),
    Selectable (..),
    elems,
    insertElemsWithResolution,
    member,
    mergeWithResolution,
  )
import Data.Morpheus.Types.Internal.AST.Base
  ( Ref,
    ValidationErrors,
  )
import Data.Morpheus.Types.Internal.AST.OrdMap
  ( OrdMap (..),
  )
import qualified Data.Morpheus.Types.Internal.AST.OrdMap as OM
import Data.Morpheus.Types.Internal.AST.Stage
  ( RAW,
    Stage,
    VALID,
  )
import Data.Semigroup (Semigroup (..))
import Data.Traversable (Traversable (..))
import Language.Haskell.TH.Syntax (Lift (..))
import Prelude
  ( ($),
    (.),
    Eq (..),
    Show,
    otherwise,
  )

-- set with mergeable components
newtype MergeSet (dups :: Stage) a = MergeSet
  { unpack :: [a]
  }
  deriving
    ( Show,
      Eq,
      Functor,
      Foldable,
      Lift,
      Traversable,
      Collection a
    )

concatTraverse ::
  ( Eq a,
    Eq b,
    Merge a,
    Merge b,
    KeyOf k b,
    Monad m,
    Failure ValidationErrors m
  ) =>
  (a -> m (MergeSet VALID b)) ->
  MergeSet RAW a ->
  m (MergeSet VALID b)
concatTraverse f smap =
  traverse f (elems smap)
    >>= join

join ::
  ( Eq a,
    KeyOf k a,
    Merge a,
    Monad m,
    Failure ValidationErrors m,
    Listable a (MergeSet opt a),
    Merge (MergeSet opt a)
  ) =>
  [MergeSet opt a] ->
  m (MergeSet opt a)
join = __join empty
  where
    __join acc [] = pure acc
    __join acc (x : xs) = acc <:> x >>= (`__join` xs)

toOrdMap :: (KeyOf k a) => MergeSet opt a -> OrdMap k a
toOrdMap = OM.unsafeFromValues . unpack

instance (KeyOf k a) => Selectable k a (MergeSet opt a) where
  selectOr fb f key (MergeSet ls) = maybe fb f (find ((key ==) . keyOf) ls)

-- must merge files on collision

instance
  ( KeyOf k a,
    Listable a (MergeSet VALID a),
    Merge a,
    Eq a
  ) =>
  Merge (MergeSet VALID a)
  where
  merge = safeJoin

instance
  ( Listable a (MergeSet VALID a),
    KeyOf k a,
    Merge a,
    Eq a
  ) =>
  Listable a (MergeSet VALID a)
  where
  fromElems = safeFromList
  elems = unpack

instance Merge (MergeSet RAW a) where
  merge _ (MergeSet x) (MergeSet y) = pure $ MergeSet $ x <> y

instance Listable a (MergeSet RAW a) where
  fromElems = pure . MergeSet
  elems = unpack

safeFromList :: (Monad m, Eq a, KeyOf k a, Merge a, Failure ValidationErrors m) => [a] -> m (MergeSet opt a)
safeFromList = insertElemsWithResolution upsert (resolveConflict []) empty

safeJoin :: (Monad m, KeyOf k a, Eq a, Listable a (MergeSet opt a), Merge a, Failure ValidationErrors m) => [Ref] -> MergeSet opt a -> MergeSet opt a -> m (MergeSet opt a)
safeJoin path = mergeWithResolution upsert (resolveConflict path)

upsert :: (KeyOf k a) => a -> MergeSet opt a -> MergeSet opt a
upsert value (MergeSet values)
  | keyOf value `member` values = MergeSet (fmap replaceBy values)
  | otherwise = MergeSet (values <> [value])
  where
    replaceBy oldValue
      | keyOf value == keyOf oldValue = value
      | otherwise = oldValue

resolveConflict :: (Monad m, Eq a, KeyOf k a, Merge a, Failure ValidationErrors m) => [Ref] -> a -> a -> m a
resolveConflict path oldValue newValue
  | oldValue == newValue = pure oldValue
  | otherwise = merge path oldValue newValue

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Morpheus.Types.Internal.AST.MergeSet
  ( MergeSet,
    toOrderedMap,
    concatTraverse,
    join,
  )
where

import Data.List ((\\), find)
import Data.Maybe (maybe)
-- MORPHEUS

import Data.Morpheus.Internal.Utils
  ( (<:>),
    Collection (..),
    Failure (..),
    KeyOf (..),
    Listable (..),
    Merge (..),
    Selectable (..),
    elems,
  )
import Data.Morpheus.Types.Internal.AST.Base
  ( FieldName,
    GQLErrors,
    Ref,
  )
import Data.Morpheus.Types.Internal.AST.OrderedMap
  ( OrderedMap (..),
  )
import qualified Data.Morpheus.Types.Internal.AST.OrderedMap as OM
import Data.Semigroup ((<>))
import Data.String (IsString)
import Language.Haskell.TH.Syntax (Lift (..))

-- set with mergeable components
newtype MergeSet a = MergeSet
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
  ( KeyOf a,
    Eq a,
    Eq b,
    KEY a ~ FieldName,
    Merge a,
    Merge b,
    KeyOf b,
    Monad m,
    IsString (KEY a),
    IsString (KEY b),
    Failure GQLErrors m
  ) =>
  (a -> m (MergeSet b)) ->
  MergeSet a ->
  m (MergeSet b)
concatTraverse f smap = traverse f (elems smap) >>= join

join ::
  ( Eq a,
    KeyOf a,
    Merge a,
    Monad m,
    IsString (KEY a),
    Failure GQLErrors m
  ) =>
  [MergeSet a] ->
  m (MergeSet a)
join = __join empty
  where
    __join acc [] = pure acc
    __join acc (x : xs) = acc <:> x >>= (`__join` xs)

toOrderedMap :: (KEY a ~ FieldName, KeyOf a) => MergeSet a -> OrderedMap FieldName a
toOrderedMap = OM.unsafeFromValues . unpack

instance (KeyOf a, k ~ KEY a, IsString k) => Selectable (MergeSet a) a where
  selectOr fb _ "" _ = fb
  selectOr fb f key (MergeSet ls) = maybe fb f (find ((key ==) . keyOf) ls)

-- must merge files on collision
instance (KeyOf a, IsString (KEY a), Merge a, Eq a) => Merge (MergeSet a) where
  merge = safeJoin

instance (KeyOf a, Merge a, IsString (KEY a), Eq a) => Listable a (MergeSet a) where
  fromElems = safeFromList
  elems = unpack

safeFromList :: (Monad m, KeyOf a, IsString (KEY a), Eq a, Merge a, Failure GQLErrors m) => [a] -> m (MergeSet a)
safeFromList = insertList [] empty

safeJoin :: (Monad m, KeyOf a, Eq a, IsString (KEY a), Merge a, Failure GQLErrors m) => [Ref] -> MergeSet a -> MergeSet a -> m (MergeSet a)
safeJoin path hm1 hm2 = insertList path hm1 (elems hm2)

insertList :: (Monad m, Eq a, KeyOf a, IsString (KEY a), Merge a, Failure GQLErrors m) => [Ref] -> MergeSet a -> [a] -> m (MergeSet a)
insertList _ smap [] = pure smap
insertList path smap (x : xs) = insert path smap x >>= flip (insertList path) xs

insert :: (Monad m, Eq a, IsString (KEY a), KeyOf a, Merge a, Failure GQLErrors m) => [Ref] -> MergeSet a -> a -> m (MergeSet a)
insert path mSet@(MergeSet ls) currentValue = MergeSet <$> __insert
  where
    __insert =
      selectOr
        (pure $ ls <> [currentValue])
        mergeWith
        (keyOf currentValue)
        mSet
    ------------------
    mergeWith oldValue
      | oldValue == currentValue = pure ls
      | otherwise = do
        mergedValue <- merge path oldValue currentValue
        pure $ (ls \\ [oldValue]) <> [mergedValue]

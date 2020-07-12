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
import Data.Functor ((<$>), Functor (..))
import Data.List ((\\), find)
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
  )
import Data.Morpheus.Types.Internal.AST.Base
  ( FieldName,
    GQLErrors,
    Ref,
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
    flip,
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
    KeyOf b,
    Monad m,
    Failure GQLErrors m
  ) =>
  (a -> m (MergeSet VALID b)) ->
  MergeSet RAW a ->
  m (MergeSet VALID b)
concatTraverse f smap =
  traverse f (elems smap)
    >>= join

join ::
  ( Eq a,
    KeyOf a,
    Merge a,
    Monad m,
    Failure GQLErrors m,
    Listable a (MergeSet opt a),
    Merge (MergeSet opt a)
  ) =>
  [MergeSet opt a] ->
  m (MergeSet opt a)
join = __join empty
  where
    __join acc [] = pure acc
    __join acc (x : xs) = acc <:> x >>= (`__join` xs)

toOrdMap :: (KEY a ~ FieldName, KeyOf a) => MergeSet opt a -> OrdMap FieldName a
toOrdMap = OM.unsafeFromValues . unpack

instance (KeyOf a, k ~ KEY a) => Selectable a (MergeSet opt a) where
  selectOr fb f key (MergeSet ls) = maybe fb f (find ((key ==) . keyOf) ls)

-- must merge files on collision

instance
  ( KeyOf a,
    Listable a (MergeSet VALID a),
    Merge a,
    Eq a
  ) =>
  Merge (MergeSet VALID a)
  where
  merge = safeJoin

instance
  ( Listable a (MergeSet VALID a),
    KeyOf a,
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

safeFromList :: (Monad m, KeyOf a, Eq a, Merge a, Failure GQLErrors m) => [a] -> m (MergeSet opt a)
safeFromList = insertList [] empty

safeJoin :: (Monad m, KeyOf a, Eq a, Listable a (MergeSet opt a), Merge a, Failure GQLErrors m) => [Ref] -> MergeSet opt a -> MergeSet opt a -> m (MergeSet opt a)
safeJoin path hm1 hm2 = insertList path hm1 (elems hm2)

insertList :: (Monad m, Eq a, KeyOf a, Merge a, Failure GQLErrors m) => [Ref] -> MergeSet opt a -> [a] -> m (MergeSet opt a)
insertList _ smap [] = pure smap
insertList path smap (x : xs) = insert path smap x >>= flip (insertList path) xs

insert :: (Monad m, Eq a, KeyOf a, Merge a, Failure GQLErrors m) => [Ref] -> MergeSet opt a -> a -> m (MergeSet opt a)
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

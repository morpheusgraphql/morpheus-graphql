{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.Stitching
  ( Stitching (..),
  )
where

import Control.Applicative (Applicative (..))
import Control.Monad (Monad (..))
import Data.Functor ((<$>))
import Data.Maybe (Maybe (..))
import Data.Morpheus.Error.NameCollision (NameCollision (..))
import Data.Morpheus.Ext.Map
  ( resolveWith,
    runResolutionT,
  )
import qualified Data.Morpheus.Ext.OrdMap as OM
import qualified Data.Morpheus.Ext.SafeHashMap as SHM
import Data.Morpheus.Ext.SemigroupM (SemigroupM (..))
import Data.Morpheus.Internal.Utils
  ( Failure (..),
    mergeT,
    prop,
  )
import Data.Morpheus.Types.Internal.AST
  ( Directive,
    DirectiveDefinition,
    FieldDefinition,
    FieldsDefinition,
    Schema (..),
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeLib,
    ValidationErrors,
  )
import Data.Morpheus.Types.Internal.Resolving (RootResModel)
import qualified Data.Morpheus.Types.Internal.Resolving as R (RootResModel (..))
import Data.Semigroup (Semigroup (..))
import Prelude
  ( ($),
    (.),
    Eq (..),
    otherwise,
  )

equal :: (Eq a, Applicative m, Failure ValidationErrors m) => ValidationErrors -> a -> a -> m a
equal err p1 p2
  | p1 == p2 = pure p2
  | otherwise = failure err

fstM :: Applicative m => a -> a -> m a
fstM x _ = pure x

concatM :: (Applicative m, Semigroup a) => a -> a -> m a
concatM x = pure . (x <>)

class Stitching a where
  stitch :: (Monad m, Failure ValidationErrors m) => a -> a -> m a

instance Stitching a => Stitching (Maybe a) where
  stitch = optional stitch

instance Stitching (Schema s) where
  stitch s1 s2 =
    Schema
      <$> prop stitch types s1 s2
      <*> prop stitchOperation query s1 s2
      <*> prop (optional stitchOperation) mutation s1 s2
      <*> prop (optional stitchOperation) subscription s1 s2
      <*> prop stitch directiveDefinitions s1 s2

instance Stitching (TypeLib s) where
  stitch x y = runResolutionT (mergeT x y) SHM.unsafeFromList (resolveWith stitch)

instance Stitching [DirectiveDefinition s] where
  stitch = concatM

instance Stitching [Directive s] where
  stitch = concatM

optional :: Applicative f => (t -> t -> f t) -> Maybe t -> Maybe t -> f (Maybe t)
optional _ Nothing y = pure y
optional _ (Just x) Nothing = pure (Just x)
optional f (Just x) (Just y) = Just <$> f x y

stitchOperation ::
  (Monad m, Failure ValidationErrors m) =>
  TypeDefinition c s ->
  TypeDefinition c s ->
  m (TypeDefinition c s)
stitchOperation x y =
  TypeDefinition
    <$> prop concatM typeDescription x y
    <*> prop fstM typeName x y
    <*> prop stitch typeDirectives x y
    <*> prop stitch typeContent x y

instance Stitching (TypeDefinition cat s) where
  stitch x y =
    TypeDefinition
      <$> prop concatM typeDescription x y
      <*> prop (equal [nameCollision y]) typeName x y
      <*> prop stitch typeDirectives x y
      <*> prop stitch typeContent x y

instance Stitching (TypeContent TRUE cat s) where
  stitch (DataObject i1 fields1) (DataObject i2 fields2) =
    DataObject (i1 <> i2) <$> stitch fields1 fields2
  stitch _ _ = failure (["Schema Stitching works only for objects"] :: ValidationErrors)

instance Stitching (FieldsDefinition cat s) where
  stitch x y = runResolutionT (mergeT x y) OM.unsafeFromList (resolveWith stitch)

instance Stitching (FieldDefinition cat s) where
  stitch old new
    | old == new = pure old
    | otherwise = failure [nameCollision new]

rootProp :: (Monad m, SemigroupM m b) => (a -> m b) -> a -> a -> m b
rootProp f x y = do
  x' <- f x
  y' <- f y
  mergeM [] x' y'

stitchSubscriptions :: Failure ValidationErrors m => Maybe a -> Maybe a -> m (Maybe a)
stitchSubscriptions Just {} Just {} = failure (["can't merge  subscription applications"] :: ValidationErrors)
stitchSubscriptions x Nothing = pure x
stitchSubscriptions Nothing x = pure x

instance Monad m => Stitching (RootResModel e m) where
  stitch x y = do
    channelMap <- stitchSubscriptions (R.channelMap x) (R.channelMap y)
    pure $
      R.RootResModel
        { R.query = rootProp R.query x y,
          R.mutation = rootProp R.mutation x y,
          R.subscription = rootProp R.subscription x y,
          R.channelMap
        }

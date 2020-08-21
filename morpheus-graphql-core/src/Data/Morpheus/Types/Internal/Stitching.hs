{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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
import Data.Morpheus.Internal.Utils
  ( Failure (..),
    mergeT,
    prop,
    runResolutionT,
  )
import Data.Morpheus.Types.Internal.AST
  ( Directive,
    DirectiveDefinition,
    FieldDefinition,
    Fields (..),
    FieldsDefinition,
    Schema (..),
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeLib,
    ValidationErrors,
  )
import qualified Data.Morpheus.Types.Internal.AST.OrdMap as OM (upsert)
import qualified Data.Morpheus.Types.Internal.AST.SafeHashMap as SHM (upsert)
import Data.Morpheus.Types.Internal.Resolving (RootResModel)
import Data.Semigroup (Semigroup (..))
import Prelude
  ( (.),
    Eq (..),
    otherwise,
  )

equal :: (Eq a, Applicative m, Failure ValidationErrors m) => ValidationErrors -> a -> a -> m a
equal err p1 p2
  | p1 == p2 = pure p2
  | otherwise = failure err

concatM :: (Applicative m, Semigroup a) => a -> a -> m a
concatM x = pure . (x <>)

class Stitching a where
  stitch :: (Monad m, Failure ValidationErrors m) => a -> a -> m a

instance Stitching a => Stitching (Maybe a) where
  stitch Nothing y = pure y
  stitch (Just x) Nothing = pure (Just x)
  stitch (Just x) (Just y) = Just <$> stitch x y

instance Stitching (Schema s) where
  stitch s1 s2 =
    Schema
      <$> prop stitch types s1 s2
      <*> prop stitch query s1 s2
      <*> prop stitch mutation s1 s2
      <*> prop stitch subscription s1 s2
      <*> prop stitch directiveDefinitions s1 s2

instance Stitching (TypeLib s) where
  stitch x y = runResolutionT (mergeT x y) SHM.upsert stitch

instance Stitching [DirectiveDefinition s] where
  stitch = concatM

instance Stitching [Directive s] where
  stitch = concatM

instance Stitching (TypeDefinition cat s) where
  stitch x y =
    TypeDefinition
      <$> prop (equal [nameCollision y]) typeName x y
      <*> prop (equal [nameCollision y]) typeFingerprint x y
      <*> prop concatM typeDescription x y
      <*> prop stitch typeDirectives x y
      <*> prop stitch typeContent x y

instance Stitching (TypeContent TRUE cat s) where
  stitch (DataObject i1 fields1) (DataObject i2 fields2) =
    DataObject (i1 <> i2) <$> stitch fields1 fields2
  stitch _ _ = failure (["Schema Stitching works only for objects"] :: ValidationErrors)

instance Stitching (FieldsDefinition cat s) where
  stitch (Fields x) (Fields y) = Fields <$> runResolutionT (mergeT x y) OM.upsert stitch

instance Stitching (FieldDefinition cat s) where
  stitch old new
    | old == new = pure old
    | otherwise = failure [nameCollision new]

instance Stitching (RootResModel e m) where
  stitch _ = pure

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
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
    mergeWithResolution,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    DirectiveDefinition,
    FieldDefinition,
    Fields (..),
    FieldsDefinition,
    OBJECT,
    Schema (..),
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeLib,
    ValidationErrors,
  )
import qualified Data.Morpheus.Types.Internal.AST.OrdMap as OM (upsert)
import qualified Data.Morpheus.Types.Internal.AST.SafeHashMap as SHM (upsert)
import Data.Semigroup ((<>))
import Prelude
  ( ($),
    (.),
    Eq (..),
    otherwise,
  )

optional ::
  ( Monad m,
    Failure ValidationErrors m,
    Stitching a
  ) =>
  Maybe a ->
  Maybe a ->
  m (Maybe a)
optional Nothing y = pure y
optional (Just x) Nothing = pure (Just x)
optional (Just x) (Just y) = Just <$> stitch x y

prop :: (b -> b -> m b) -> (a -> b) -> a -> a -> m b
prop f fSel a1 a2 = f (fSel a1) (fSel a2)

class Stitching a where
  stitch :: (Monad m, Failure ValidationErrors m) => a -> a -> m a

instance Stitching (Schema s) where
  stitch s1 s2 =
    Schema
      <$> prop stitch types s1 s2
      <*> prop stitch query s1 s2
      <*> prop optional mutation s1 s2
      <*> prop optional subscription s1 s2
      <*> prop stitch directiveDefinitions s1 s2

instance Stitching (TypeLib s) where
  stitch = mergeWithResolution SHM.upsert stitch

instance Stitching [DirectiveDefinition s] where
  stitch x = pure . (x <>)

instance Stitching (TypeDefinition ANY s) where
  stitch x y = failure [nameCollision y]

instance Stitching (TypeDefinition OBJECT s) where
  stitch ty1 TypeDefinition {typeContent = cont2, ..} = do
    cont <- stitch (typeContent ty1) cont2
    pure $ TypeDefinition {typeContent = cont, ..}

instance Stitching (TypeContent TRUE OBJECT s) where
  stitch (DataObject i1 fields1) (DataObject i2 fields2) =
    DataObject (i1 <> i2) <$> stitch fields1 fields2

instance Stitching (FieldsDefinition cat s) where
  stitch (Fields x) (Fields y) = Fields <$> mergeWithResolution OM.upsert stitch x y

instance Stitching (FieldDefinition cat s) where
  stitch old new
    | old == new = pure old
    | otherwise = failure [nameCollision new]

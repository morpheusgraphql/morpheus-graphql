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
  ( (<:>),
    Failure (..),
    KeyOf (..),
    Listable (..),
    selectOr,
  )
import Data.Morpheus.Types.Internal.AST
  ( DirectiveDefinition,
    FieldDefinition,
    Fields (..),
    FieldsDefinition,
    OBJECT,
    OrdMap (..),
    Schema (..),
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeLib,
    ValidationErrors,
  )
import Data.Morpheus.Types.Internal.AST.OrdMap (upsert)
import Data.Semigroup ((<>))
import Prelude
  ( ($),
    (.),
    Eq (..),
    flip,
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
  stitch = (<:>)

instance Stitching [DirectiveDefinition s] where
  stitch x = pure . (x <>)

instance Stitching (TypeDefinition OBJECT s) where
  stitch ty1 TypeDefinition {typeContent = cont2, ..} = do
    cont <- stitch (typeContent ty1) cont2
    pure $ TypeDefinition {typeContent = cont, ..}

instance Stitching (TypeContent TRUE OBJECT s) where
  stitch (DataObject i1 fields1) (DataObject i2 fields2) =
    DataObject (i1 <> i2) <$> stitch fields1 fields2

instance Stitching (FieldsDefinition cat s) where
  stitch (Fields x) (Fields y) = Fields <$> safeJoin x y

instance Stitching (FieldDefinition cat s) where
  stitch old new
    | old == new = pure old
    | otherwise = failure [nameCollision new]

safeJoin :: (Monad m, KeyOf k a, Stitching a, Listable a (OrdMap k a), Failure ValidationErrors m) => OrdMap k a -> OrdMap k a -> m (OrdMap k a)
safeJoin hm1 hm2 = insertList hm1 (elems hm2)

insertList :: (Monad m, KeyOf k a, Stitching a, Failure ValidationErrors m) => OrdMap k a -> [a] -> m (OrdMap k a)
insertList smap [] = pure smap
insertList smap (x : xs) = insert x smap >>= flip insertList xs

insert :: (Monad m, KeyOf k a, Stitching a, Failure ValidationErrors m) => a -> OrdMap k a -> m (OrdMap k a)
insert value ordSet =
  selectOr
    (pure $ upsert value ordSet)
    stitchValue
    (keyOf value)
    ordSet
  where
    stitchValue oldValue = (`upsert` ordSet) <$> stitch oldValue value
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
import Control.Monad (Monad)
import Data.Functor ((<$>))
import Data.Maybe (Maybe (..))
import Data.Morpheus.Internal.Utils
  ( (<:>),
    Failure,
  )
import Data.Morpheus.Types.Internal.AST
  ( DirectiveDefinition,
    FieldsDefinition,
    OBJECT,
    OUT,
    Schema (..),
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeLib,
    ValidationErrors,
  )
import Data.Semigroup ((<>))
import Prelude (($), (.))

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

class Stitching a where
  stitch :: (Monad m, Failure ValidationErrors m) => a -> a -> m a

instance Stitching (Schema s) where
  stitch s1 s2 =
    Schema
      <$> stitch (types s1) (types s2)
      <*> stitch (query s1) (query s2)
      <*> optional (mutation s1) (mutation s2)
      <*> optional (subscription s1) (subscription s2)
      <*> stitch (directiveDefinitions s1) (directiveDefinitions s2)

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

instance Stitching (FieldsDefinition OUT s) where
  stitch = (<:>)

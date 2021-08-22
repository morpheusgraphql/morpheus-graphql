{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.App.Internal.Stitching
  ( Stitching (..),
  )
where

import Control.Monad.Except (MonadError (throwError))
import qualified Data.Morpheus.App.Internal.Resolving as R (RootResolverValue (..))
import Data.Morpheus.App.Internal.Resolving (TypeResolver (..))
import qualified Data.Morpheus.App.Internal.Resolving.Utils as R
import Data.Morpheus.Error (NameCollision (..))
import Data.Morpheus.Internal.Ext
  ( Merge (merge),
    resolveWith,
    runResolutionT,
    unsafeFromList,
  )
import Data.Morpheus.Internal.Utils
  ( mergeT,
    prop,
  )
import Data.Morpheus.Types.Internal.AST
  ( Directives,
    DirectivesDefinition,
    FieldDefinition,
    FieldsDefinition,
    GQLError,
    Schema (..),
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeDefinitions,
  )
import Relude hiding (optional)

equal :: (Eq a, Applicative m, MonadError GQLError m) => GQLError -> a -> a -> m a
equal err p1 p2
  | p1 == p2 = pure p2
  | otherwise = throwError err

fstM :: Applicative m => a -> a -> m a
fstM x _ = pure x

concatM :: (Applicative m, Semigroup a) => a -> a -> m a
concatM x = pure . (x <>)

class Stitching a where
  stitch :: (Monad m, MonadError GQLError m) => a -> a -> m a

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

instance Stitching (TypeDefinitions s) where
  stitch x y = runResolutionT (mergeT x y) unsafeFromList (resolveWith stitch)

instance Stitching (DirectivesDefinition s) where
  stitch = merge

instance Stitching (Directives s) where
  stitch = merge

optional :: Applicative f => (t -> t -> f t) -> Maybe t -> Maybe t -> f (Maybe t)
optional _ Nothing y = pure y
optional _ (Just x) Nothing = pure (Just x)
optional f (Just x) (Just y) = Just <$> f x y

stitchOperation ::
  (Monad m, MonadError GQLError m) =>
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
      <*> prop (equal $ nameCollision y) typeName x y
      <*> prop stitch typeDirectives x y
      <*> prop stitch typeContent x y

instance Stitching (TypeContent TRUE cat s) where
  stitch (DataObject i1 fields1) (DataObject i2 fields2) =
    DataObject (i1 <> i2) <$> stitch fields1 fields2
  stitch x y
    | x == y = pure y
    | otherwise = throwError ("Schema Stitching works only for objects" :: GQLError)

instance Stitching (FieldsDefinition cat s) where
  stitch x y = runResolutionT (mergeT x y) unsafeFromList (resolveWith stitch)

instance Stitching (FieldDefinition cat s) where
  stitch old new
    | old == new = pure old
    | otherwise = throwError $ nameCollision new

rootProp :: (Monad m, Merge m b) => (a -> m b) -> a -> a -> m b
rootProp f x y = do
  x' <- f x
  y' <- f y
  merge x' y'

stitchSubscriptions :: MonadError GQLError m => Maybe a -> Maybe a -> m (Maybe a)
stitchSubscriptions Just {} Just {} = throwError ("can't merge  subscription applications" :: GQLError)
stitchSubscriptions x Nothing = pure x
stitchSubscriptions Nothing x = pure x

instance Stitching (R.ObjectTypeResolver (m a)) where
  stitch t1 t2
    | R.__typename t1 == R.__typename t2 =
      pure $
        R.ObjectTypeResolver
          (R.__typename t1)
          ( R.objectFields t1 <> R.objectFields t2
          )
    | otherwise = throwError "typeResolvers must have same resolverName"

instance (MonadError GQLError m) => Stitching (TypeResolver m) where
  stitch t1 t2
    | resolverName t1 == resolverName t2 =
      pure
        TypeResolver
          { resolverName = resolverName t1,
            resolver = \arg -> do
              t1' <- resolver t1 arg
              t2' <- resolver t2 arg
              stitch t1' t2'
          }
    | otherwise = throwError "typeResolvers must have same resolverName"

instance Monad m => Stitching (R.RootResolverValue e m) where
  stitch x@R.RootResolverValue {} y@R.RootResolverValue {} = do
    channelMap <- stitchSubscriptions (R.channelMap x) (R.channelMap y)
    pure $
      R.RootResolverValue
        { R.query = rootProp R.query x y,
          R.mutation = rootProp R.mutation x y,
          R.subscription = rootProp R.subscription x y,
          R.channelMap
        }
  stitch
    R.TypeResolversValue
      { R.queryTypeResolvers = q1,
        R.mutationTypeResolvers,
        R.subscriptionTypeResolvers,
        R.typeResolverChannels
      }
    R.TypeResolversValue
      { R.queryTypeResolvers = q2
      } =
      do
        result <- runResolutionT (mergeT q1 q2) unsafeFromList (resolveWith stitch)
        pure
          ( R.TypeResolversValue
              { R.queryTypeResolvers = result,
                R.mutationTypeResolvers,
                R.subscriptionTypeResolvers,
                R.typeResolverChannels
              }
          )
  stitch _ _ = throwError "only apps with same resolver model can be merged"

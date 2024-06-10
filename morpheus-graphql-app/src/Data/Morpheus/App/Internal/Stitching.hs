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
import Data.Morpheus.App.Internal.Resolving (RootResolverValue (..))
import Data.Morpheus.App.Internal.Resolving.Types
  ( NamedResolver (..),
    NamedResolverResult (..),
  )
import qualified Data.Morpheus.App.Internal.Resolving.Types as R
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
  ( DirectiveDefinition,
    Directives,
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

fstM :: (Applicative m) => a -> a -> m a
fstM x _ = pure x

concatM :: (Applicative m, Semigroup a) => a -> a -> m a
concatM x = pure . (x <>)

class Stitching a where
  stitch :: (Monad m, MonadError GQLError m) => a -> a -> m a

instance (Stitching a) => Stitching (Maybe a) where
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
  stitch x y = runResolutionT (mergeT x y) unsafeFromList (resolveWith stitch)

instance Stitching (Directives s) where
  stitch = merge

optional :: (Applicative f) => (t -> t -> f t) -> Maybe t -> Maybe t -> f (Maybe t)
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

instance Stitching (DirectiveDefinition s) where
  stitch x y
    | x == y = pure x
    | otherwise = throwError "only directives with same structure can be merged"

instance Stitching (TypeDefinition cat s) where
  stitch x y =
    TypeDefinition
      <$> prop concatM typeDescription x y
      <*> prop (equal $ nameCollision y) typeName x y
      <*> prop stitch typeDirectives x y
      <*> prop stitch typeContent x y

instance Stitching (TypeContent TRUE cat s) where
  stitch (DataScalar _) (DataScalar x) = pure $ DataScalar x
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

stitchSubscriptions :: (MonadError GQLError m) => Maybe a -> Maybe a -> m (Maybe a)
stitchSubscriptions Just {} Just {} = throwError ("can't merge  subscription applications" :: GQLError)
stitchSubscriptions x Nothing = pure x
stitchSubscriptions Nothing x = pure x

instance Stitching (R.ObjectTypeResolver m) where
  stitch t1 t2 = pure $ R.ObjectTypeResolver (R.objectFields t1 <> R.objectFields t2)

instance (MonadError GQLError m) => Stitching (NamedResolverResult m) where
  -- TODO: app level constraint ensures that they have same re4solver function
  stitch NamedScalarResolver {} (NamedScalarResolver f) = pure (NamedScalarResolver f)
  stitch NamedEnumResolver {} (NamedEnumResolver x) = pure (NamedEnumResolver x)
  stitch NamedUnionResolver {} (NamedUnionResolver x) = pure (NamedUnionResolver x)
  stitch (NamedObjectResolver t1) (NamedObjectResolver t2) = NamedObjectResolver <$> stitch t1 t2
  -- NUll
  stitch NamedNullResolver x = pure x
  stitch x NamedNullResolver = pure x
  stitch _ _ = throwError "ResolverMap must have same Kind"

instance (MonadError GQLError m) => Stitching (NamedResolver m) where
  stitch t1 t2
    | resolverName t1 == resolverName t2 =
        pure
          NamedResolver
            { resolverName = resolverName t1,
              resolverFun = \arg -> do
                t1' <- resolverFun t1 arg
                t2' <- resolverFun t2 arg
                let xs = zip t1' t2'
                traverse (uncurry stitch) xs
            }
    | otherwise = throwError "ResolverMap must have same resolverName"

instance (Monad m) => Stitching (RootResolverValue e m) where
  stitch x@RootResolverValue {} y@RootResolverValue {} = do
    channelMap <- stitchSubscriptions (channelMap x) (channelMap y)
    pure
      $ RootResolverValue
        { queryResolver = rootProp queryResolver x y,
          mutationResolver = rootProp mutationResolver x y,
          subscriptionResolver = rootProp subscriptionResolver x y,
          channelMap
        }
  stitch
    NamedResolversValue
      { queryResolverMap = q1
      }
    NamedResolversValue
      { queryResolverMap = q2
      } =
      do
        result <- runResolutionT (mergeT q1 q2) unsafeFromList (resolveWith stitch)
        pure (NamedResolversValue {queryResolverMap = result})
  stitch _ _ = throwError "only apps with same resolver model can be merged"

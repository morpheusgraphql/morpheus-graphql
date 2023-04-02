{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.App.Internal.Resolving.ResolveValue
  ( resolvePlainRoot,
    resolveNamedRoot,
  )
where

import Control.Monad.Except (MonadError (throwError))
import Data.Morpheus.App.Internal.Resolving.Batching
  ( NamedContext (..),
    ResolverMapT,
    resolveRef,
    runResMapT,
    setCache,
  )
import Data.Morpheus.App.Internal.Resolving.Cache (CacheValue (..))
import Data.Morpheus.App.Internal.Resolving.MonadResolver (MonadResolver)
import Data.Morpheus.App.Internal.Resolving.ResolverState
  ( ResolverContext (..),
    inSelectionField,
  )
import Data.Morpheus.App.Internal.Resolving.Types
  ( ResolverMap,
    mkEnum,
    mkNull,
    mkString,
    mkUnion,
  )
import Data.Morpheus.App.Internal.Resolving.Utils
import Data.Morpheus.Internal.Utils
  ( KeyOf (keyOf),
    empty,
    traverseCollection,
  )
import Data.Morpheus.Types.Internal.AST
  ( ObjectEntry (ObjectEntry),
    ScalarValue (..),
    Selection (..),
    SelectionContent (..),
    SelectionSet,
    TypeDefinition (..),
    TypeName,
    VALID,
    ValidValue,
    Value (..),
    internal,
    unitFieldName,
    unitTypeName,
    unpackName,
  )
import Relude hiding (empty)

-- UNCACHED

resolvePlainRoot :: MonadResolver m => ObjectTypeResolver m -> SelectionSet VALID -> m ValidValue
resolvePlainRoot resolver selection = do
  name <- asks (typeName . currentType)
  runResMapT (resolveSelection (SelectionSet selection) (ResObject (Just name) resolver)) (NamedContext empty empty)

-- CACHED
resolveNamedRoot :: MonadResolver m => TypeName -> ResolverMap m -> SelectionSet VALID -> m ValidValue
resolveNamedRoot typeName resolvers selection =
  runResMapT
    (resolveSelection (SelectionSet selection) (ResRef $ pure (NamedResolverRef typeName ["ROOT"])))
    (NamedContext empty resolvers)

-- RESOLVING

resolveSelection :: (MonadResolver m) => SelectionContent VALID -> ResolverValue m -> ResolverMapT m ValidValue
resolveSelection selection (ResLazy x) = lift x >>= resolveSelection selection
resolveSelection selection (ResList xs) = List <$> traverse (resolveSelection selection) xs
resolveSelection SelectionField (ResEnum name) = pure $ Scalar $ String $ unpackName name
resolveSelection selection@UnionSelection {} (ResEnum name) = resolveSelection selection (mkUnion name [(unitFieldName, pure $ mkEnum unitTypeName)])
resolveSelection _ ResEnum {} = throwError (internal "wrong selection on enum value")
resolveSelection _ ResNull = pure Null
resolveSelection SelectionField (ResScalar x) = pure $ Scalar x
resolveSelection _ ResScalar {} = throwError (internal "scalar resolver should only receive SelectionField")
resolveSelection selection (ResObject typeName obj) = withObject typeName (mapSelectionSet resolveField) selection
  where
    resolveField s = lift (toResolverValue obj s) >>= resolveSelection (selectionContent s)
resolveSelection selection (ResRef mRef) = do
  (value, cache) <- resolveRef selection =<< lift mRef
  setCache cache $
    case value of
      (CachedValue v) -> pure v
      (CachedResolver v) -> resolveSelection selection v

toResolverValue ::
  (MonadResolver m) =>
  ObjectTypeResolver m ->
  Selection VALID ->
  m (ResolverValue m)
toResolverValue obj Selection {selectionName}
  | selectionName == "__typename" = mkString . unpackName <$> asks (typeName . currentType)
  | otherwise = withField mkNull id selectionName obj

mapSelectionSet :: (ResolverMonad m) => (Selection VALID -> m ValidValue) -> Maybe (SelectionSet VALID) -> m ValidValue
mapSelectionSet f = fmap Object . maybe (pure empty) (traverseCollection (\sel -> ObjectEntry (keyOf sel) <$> inSelectionField sel (f sel)))

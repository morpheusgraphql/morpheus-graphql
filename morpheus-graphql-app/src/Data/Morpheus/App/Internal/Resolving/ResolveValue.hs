{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
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
  ( MonadBatching (..),
    runBatchedT,
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
resolvePlainRoot :: (MonadResolver m) => ObjectTypeResolver m -> SelectionSet VALID -> m ValidValue
resolvePlainRoot resolver selection = do
  name <- asks (typeName . currentType)
  runIdentityT (resolveSelection (SelectionSet selection) (ResObject (Just name) resolver))

-- CACHED
resolveNamedRoot :: (MonadResolver m) => TypeName -> ResolverMap m -> SelectionSet VALID -> m ValidValue
resolveNamedRoot typeName resolvers selection =
  runBatchedT
    (resolveSelection (SelectionSet selection) (ResRef $ pure (NamedResolverRef typeName ["ROOT"])))
    resolvers

-- RESOLVING

resolveSelection :: (ResolverMonad (t m), MonadBatching t, MonadResolver m) => SelectionContent VALID -> ResolverValue m -> t m ValidValue
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
  (key, value) <- resolveRef selection =<< lift mRef
  case value of
    (CachedValue v) -> pure v
    (CachedResolver v) -> resolveSelection selection v >>= storeValue key

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

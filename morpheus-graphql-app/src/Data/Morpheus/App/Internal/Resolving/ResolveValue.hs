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
  ( ResolverMapContext (..),
    ResolverMapT,
    SelectionRef,
    lookupResolvers,
    runResMapT,
    withBatching,
  )
import Data.Morpheus.App.Internal.Resolving.MonadResolver (MonadResolver)
import Data.Morpheus.App.Internal.Resolving.Refs
import Data.Morpheus.App.Internal.Resolving.ResolverState
  ( ResolverContext (..),
    inSelectionField,
  )
import Data.Morpheus.App.Internal.Resolving.Types
  ( NamedResolverResult (..),
    ResolverMap,
    mkEnum,
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
resolvePlainRoot root selection = runResMapT (resolveObject root (Just selection)) (ResolverMapContext mempty mempty)

-- CACHED
resolveNamedRoot :: MonadResolver m => ResolverMap m -> SelectionSet VALID -> m ValidValue
resolveNamedRoot resolvers selection =
  runResMapT
    (resolveSelection (SelectionSet selection) (ResRef $ pure (NamedResolverRef "Query" ["ROOT"])))
    (ResolverMapContext empty resolvers)

-- RESOLVING

resolveSelection :: (MonadResolver m) => SelectionContent VALID -> ResolverValue m -> ResolverMapT m ValidValue
resolveSelection selection (ResLazy x) = lift x >>= resolveSelection selection
resolveSelection selection (ResList xs) = List <$> traverse (resolveSelection selection) xs
resolveSelection selection (ResObject typeName obj) = withObject typeName (resolveObject obj) selection
resolveSelection SelectionField (ResEnum name) = pure $ Scalar $ String $ unpackName name
resolveSelection selection@UnionSelection {} (ResEnum name) = resolveSelection selection (mkUnion name [(unitFieldName, pure $ mkEnum unitTypeName)])
resolveSelection _ ResEnum {} = throwError (internal "wrong selection on enum value")
resolveSelection _ ResNull = pure Null
resolveSelection SelectionField (ResScalar x) = pure $ Scalar x
resolveSelection _ ResScalar {} = throwError (internal "scalar resolver should only receive SelectionField")
resolveSelection selection (ResRef mRef) = lift mRef >>= withBatching (flip resolveSelection) selection

resolveObject :: (MonadResolver m) => ObjectTypeResolver m -> Maybe (SelectionSet VALID) -> ResolverMapT m ValidValue
resolveObject drv sel = Object <$> maybe (pure empty) (traverseCollection (resolveField drv)) sel

resolveField :: MonadResolver m => ObjectTypeResolver m -> Selection VALID -> ResolverMapT m (ObjectEntry VALID)
resolveField drv selection = inSelectionField selection $ ObjectEntry (keyOf selection) <$> runFieldResolver selection drv

runFieldResolver ::
  (MonadResolver m) =>
  Selection VALID ->
  ObjectTypeResolver m ->
  ResolverMapT m ValidValue
runFieldResolver Selection {selectionName, selectionContent}
  | selectionName == "__typename" =
    const (Scalar . String . unpackName <$> lift (asks (typeName . currentType)))
  | otherwise = withField Null (lift >=> resolveSelection selectionContent) selectionName

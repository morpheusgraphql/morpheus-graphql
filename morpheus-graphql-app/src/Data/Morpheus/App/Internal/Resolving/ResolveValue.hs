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
  ( ResolverMapContext (..),
    ResolverMapT,
    runResMapT,
    withBatching,
  )
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
  runResMapT (resolveSelection (SelectionSet selection) (ResObject (Just name) resolver)) (ResolverMapContext mempty mempty)

-- CACHED
resolveNamedRoot :: MonadResolver m => TypeName -> ResolverMap m -> SelectionSet VALID -> m ValidValue
resolveNamedRoot typeName resolvers selection =
  runResMapT
    (resolveSelection (SelectionSet selection) (ResRef $ pure (NamedResolverRef typeName ["ROOT"])))
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
resolveSelection selection (ResRef mRef) = lift mRef >>= withBatching resolveSelection selection

resolveObject :: (MonadResolver m) => ObjectTypeResolver m -> Maybe (SelectionSet VALID) -> ResolverMapT m ValidValue
resolveObject drv sel = Object <$> maybe (pure empty) (traverseCollection (resolveField drv)) sel

resolveField :: MonadResolver m => ObjectTypeResolver m -> Selection VALID -> ResolverMapT m (ObjectEntry VALID)
resolveField drv sel = ObjectEntry (keyOf sel) <$> inSelectionField sel (lift (toResolverValue sel drv) >>= resolveSelection (selectionContent sel))

toResolverValue ::
  (MonadResolver m) =>
  Selection VALID ->
  ObjectTypeResolver m ->
  m (ResolverValue m)
toResolverValue Selection {selectionName} obj
  | selectionName == "__typename" = mkString . unpackName <$> asks (typeName . currentType)
  | otherwise = withField mkNull id selectionName obj

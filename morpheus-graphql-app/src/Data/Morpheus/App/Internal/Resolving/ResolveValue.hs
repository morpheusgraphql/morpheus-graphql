{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
    cachedRef,
    cachedWith,
    runResMapT,
  )
import Data.Morpheus.App.Internal.Resolving.MonadResolver (MonadResolver)
import Data.Morpheus.App.Internal.Resolving.Refs
import Data.Morpheus.App.Internal.Resolving.ResolverState
  ( ResolverContext (..),
    askFieldTypeName,
    updateCurrentType,
  )
import Data.Morpheus.App.Internal.Resolving.Types
  ( NamedResolver (..),
    NamedResolverResult (..),
    ResolverMap,
    mkEnum,
    mkUnion,
  )
import Data.Morpheus.App.Internal.Resolving.Utils
import Data.Morpheus.Internal.Utils
  ( KeyOf (keyOf),
    empty,
    selectOr,
    traverseCollection,
  )
import Data.Morpheus.Types.Internal.AST
  ( Msg (msg),
    ObjectEntry (ObjectEntry),
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

withCache :: (RefScanner res, MonadResolver m) => (res m -> RefSel res -> ResolverMapT m b) -> res m -> RefSel res -> ResolverMapT m b
withCache = cachedWith resolveUncachedNamedRef

resolveNamedRoot :: MonadResolver m => ResolverMap m -> SelectionSet VALID -> m ValidValue
resolveNamedRoot resolvers selection = runResMapT (resolveRef (SelectionSet selection, NamedResolverRef "Query" ["ROOT"])) (ResolverMapContext empty resolvers)

resolvePlainRoot :: MonadResolver m => ObjectTypeResolver m -> SelectionSet VALID -> m ValidValue
resolvePlainRoot root selection = runResMapT (resolveObj root (Just selection)) (ResolverMapContext mempty mempty)

-- RESOLVING
resolveRef :: (MonadResolver m) => SelectionRef -> ResolverMapT m ValidValue
resolveRef = cachedRef resolveUncachedNamedRef

resolveUncachedNamedRef :: (MonadResolver m) => SelectionRef -> ResolverMapT m [ValidValue]
resolveUncachedNamedRef (_, NamedResolverRef _ []) = pure empty
resolveUncachedNamedRef (selection, NamedResolverRef {..}) = do
  rmap <- asks resolverMap
  namedResolvers <- lift (selectOr notFound found resolverTypeName rmap)
  traverse (resolveSelection selection . toResolverValue resolverTypeName) namedResolvers
  where
    found :: (MonadResolver m) => NamedResolver m -> m [NamedResolverResult m]
    found = (resolverArgument &) . resolverFun
    notFound :: (MonadResolver m) => m [NamedResolverResult m]
    notFound = throwError ("resolver type " <> msg resolverTypeName <> "can't found")

toResolverValue :: (MonadResolver m) => TypeName -> NamedResolverResult m -> ResolverValue m
toResolverValue typeName (NamedObjectResolver res) = ResObject (Just typeName) res
toResolverValue _ (NamedUnionResolver unionRef) = ResRef $ pure unionRef
toResolverValue _ (NamedEnumResolver value) = ResEnum value
toResolverValue _ NamedNullResolver = ResNull
toResolverValue _ (NamedScalarResolver v) = ResScalar v

resolveSelection :: (MonadResolver m) => SelectionContent VALID -> ResolverValue m -> ResolverMapT m ValidValue
resolveSelection selection (ResLazy x) = lift x >>= resolveSelection selection
resolveSelection selection (ResList xs) = List <$> traverse (resolveSelection selection) xs
resolveSelection selection (ResObject typeName obj) = do
  ctx <- ask
  lift $ withObject typeName (resolveObject ctx obj) selection
resolveSelection SelectionField (ResEnum name) = pure $ Scalar $ String $ unpackName name
resolveSelection selection@UnionSelection {} (ResEnum name) = resolveSelection selection (mkUnion name [(unitFieldName, pure $ mkEnum unitTypeName)])
resolveSelection _ ResEnum {} = throwError (internal "wrong selection on enum value")
resolveSelection _ ResNull = pure Null
resolveSelection SelectionField (ResScalar x) = pure $ Scalar x
resolveSelection _ ResScalar {} = throwError (internal "scalar resolver should only receive SelectionField")
resolveSelection selection (ResRef mRef) = do
  ref <- lift mRef
  resolveRef (selection, ref)

resolveObject ::
  (MonadResolver m) =>
  ResolverMapContext m ->
  ObjectTypeResolver m ->
  Maybe (SelectionSet VALID) ->
  m ValidValue
resolveObject ctx drv sel = runResMapT (withCache resolveObj drv sel) ctx

resolveObj :: (MonadResolver m) => ObjectTypeResolver m -> Maybe (SelectionSet VALID) -> ResolverMapT m ValidValue
resolveObj drv sel = Object <$> maybe (pure empty) (traverseCollection (resolverWithCache drv)) sel

resolverWithCache ::
  MonadResolver m =>
  ObjectTypeResolver m ->
  Selection VALID ->
  ResolverMapT m (ObjectEntry VALID)
resolverWithCache drv currentSelection = do
  cacheCTX <- ask
  lift $ do
    t <- askFieldTypeName (selectionName currentSelection)
    updateCurrentType t $
      local (\resCTX -> resCTX {currentSelection}) $
        ObjectEntry (keyOf currentSelection)
          <$> runResMapT (runFieldResolver currentSelection drv) cacheCTX

runFieldResolver ::
  (MonadResolver m) =>
  Selection VALID ->
  ObjectTypeResolver m ->
  ResolverMapT m ValidValue
runFieldResolver Selection {selectionName, selectionContent}
  | selectionName == "__typename" =
      const (Scalar . String . unpackName <$> lift (asks (typeName . currentType)))
  | otherwise = withField Null (lift >=> resolveSelection selectionContent) selectionName

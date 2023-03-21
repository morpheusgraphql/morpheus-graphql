{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.App.Internal.Resolving.ResolveValue
  ( resolveRef,
    resolveObject,
    ResolverMapContext (..),
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

-- RESOLVING
resolveRef :: (MonadResolver m) => SelectionRef -> ResolverMapT m ValidValue
resolveRef = cachedRef resolveUncachedNamedRef

resolveUncachedNamedRef :: (MonadResolver m) => SelectionRef -> ResolverMapT m [ValidValue]
resolveUncachedNamedRef (_, NamedResolverRef _ []) = pure empty
resolveUncachedNamedRef (selection, NamedResolverRef {..}) = do
  rmap <- asks resolverMap
  namedResolvers <- lift (selectOr notFound found resolverTypeName rmap)
  traverse (resolveNamedResolverResult resolverTypeName selection) namedResolvers
  where
    found :: (MonadResolver m) => NamedResolver m -> m [NamedResolverResult m]
    found = (resolverArgument &) . resolverFun
    notFound :: (MonadResolver m) => m [NamedResolverResult m]
    notFound = throwError ("resolver type " <> msg resolverTypeName <> "can't found")

resolveNamedResolverResult ::
  (MonadResolver m) =>
  TypeName ->
  SelectionContent VALID ->
  NamedResolverResult m ->
  ResolverMapT m ValidValue
resolveNamedResolverResult typename selection (NamedObjectResolver res) = do
  ctx <- ask
  lift $ withObject (Just typename) (resolveObject ctx res) selection
resolveNamedResolverResult _ selection (NamedUnionResolver unionRef) = resolveSelection (ResRef $ pure unionRef) selection
resolveNamedResolverResult _ selection (NamedEnumResolver value) = resolveSelection (ResEnum value) selection
resolveNamedResolverResult _ selection NamedNullResolver = resolveSelection ResNull selection
resolveNamedResolverResult _ selection (NamedScalarResolver v) = resolveSelection (ResScalar v) selection

resolveSelection :: (MonadResolver m) => ResolverValue m -> SelectionContent VALID -> ResolverMapT m ValidValue
resolveSelection = withCache resolveUncachedSel
  where
    resolveUncachedSel (ResLazy x) selection = lift x >>= (`resolveSelection` selection)
    resolveUncachedSel (ResList xs) selection = List <$> traverse (`resolveSelection` selection) xs
    resolveUncachedSel (ResObject tyName obj) sel = do
      ctx <- ask
      lift $ withObject tyName (resolveObject ctx obj) sel
    resolveUncachedSel (ResEnum name) SelectionField = pure $ Scalar $ String $ unpackName name
    resolveUncachedSel (ResEnum name) unionSel@UnionSelection {} = resolveSelection (mkUnion name [(unitFieldName, pure $ mkEnum unitTypeName)]) unionSel
    resolveUncachedSel ResEnum {} _ = throwError (internal "wrong selection on enum value")
    resolveUncachedSel ResNull _ = pure Null
    resolveUncachedSel (ResScalar x) SelectionField = pure $ Scalar x
    resolveUncachedSel ResScalar {} _ = throwError (internal "scalar resolver should only receive SelectionField")
    resolveUncachedSel (ResRef mRef) sel = do
      ref <- lift mRef
      resolveRef (sel, ref)

resolveObject ::
  (MonadResolver m) =>
  ResolverMapContext m ->
  ObjectTypeResolver m ->
  Maybe (SelectionSet VALID) ->
  m ValidValue
resolveObject = refreshCache (\drv sel -> Object <$> maybe (pure empty) (traverseCollection (resolverWithCache drv)) sel)

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

refreshCache ::
  (MonadResolver m) =>
  (ObjectTypeResolver m -> Maybe (SelectionSet VALID) -> ResolverMapT m v) ->
  ResolverMapContext m ->
  ObjectTypeResolver m ->
  Maybe (SelectionSet VALID) ->
  m v
refreshCache f ctx drv sel
  -- TODO: this is workaround to fix https://github.com/morpheusgraphql/morpheus-graphql/issues/810
  -- which deactivates caching for non named resolvers. find out better long term solution
  | null (resolverMap ctx) = runResMapT (f drv sel) ctx
  | otherwise = runResMapT (withCache f drv sel) ctx

runFieldResolver ::
  (MonadResolver m) =>
  Selection VALID ->
  ObjectTypeResolver m ->
  ResolverMapT m ValidValue
runFieldResolver Selection {selectionName, selectionContent}
  | selectionName == "__typename" =
      const (Scalar . String . unpackName <$> lift (asks (typeName . currentType)))
  | otherwise = withField Null (lift >=> (`resolveSelection` selectionContent)) selectionName

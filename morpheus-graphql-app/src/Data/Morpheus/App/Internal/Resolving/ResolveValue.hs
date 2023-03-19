{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
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
    buildCache,
    genCache,
    runResMapT,
    useCached,
  )
import Data.Morpheus.App.Internal.Resolving.MonadResolver (MonadResolver)
import Data.Morpheus.App.Internal.Resolving.ResolverState
  ( ResolverContext (..),
    askFieldTypeName,
    updateCurrentType,
  )
import Data.Morpheus.App.Internal.Resolving.Types
  ( NamedResolver (..),
    NamedResolverRef (..),
    NamedResolverResult (..),
    ObjectTypeResolver (..),
    ResolverValue (..),
    mkEnum,
    mkUnion,
  )
import Data.Morpheus.Error (subfieldsNotSelected)
import Data.Morpheus.Internal.Utils
  ( IsMap (..),
    KeyOf (keyOf),
    empty,
    selectOr,
    traverseCollection,
    (<:>),
  )
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    GQLError,
    Msg (msg),
    ObjectEntry (ObjectEntry),
    ScalarValue (..),
    Selection (..),
    SelectionContent (..),
    SelectionSet,
    TypeDefinition (..),
    TypeName,
    UnionTag (unionTagSelection),
    VALID,
    ValidValue,
    Value (..),
    internal,
    unitFieldName,
    unitTypeName,
    unpackName,
  )
import Relude hiding (empty)

class RefScanner f where
  type RefSel f :: Type
  scanRefs :: (MonadResolver m) => RefSel f -> f m -> m [SelectionRef]

instance RefScanner ResolverValue where
  type RefSel ResolverValue = SelectionContent VALID
  scanRefs sel (ResList xs) = concat <$> traverse (scanRefs sel) xs
  scanRefs sel (ResLazy x) = x >>= scanRefs sel
  scanRefs sel (ResObject tyName obj) = withObject tyName (`scanRefs` obj) sel
  scanRefs sel (ResRef ref) = pure . (sel,) <$> ref
  scanRefs _ ResEnum {} = pure []
  scanRefs _ ResNull = pure []
  scanRefs _ ResScalar {} = pure []

instance RefScanner ObjectTypeResolver where
  type RefSel ObjectTypeResolver = Maybe (SelectionSet VALID)
  scanRefs Nothing _ = pure []
  scanRefs (Just sel) objRes = concat <$> traverse fieldRefs (toList sel)
    where
      fieldRefs currentSelection@Selection {..}
        | selectionName == "__typename" = pure []
        | otherwise = do
            t <- askFieldTypeName selectionName
            updateCurrentType t $
              local (\ctx -> ctx {currentSelection}) $ do
                x <- withField [] (fmap pure) selectionName objRes
                concat <$> traverse (scanRefs selectionContent) x

withCache :: (RefScanner res, MonadResolver m) => (res m -> RefSel res -> ResolverMapT m b) -> res m -> RefSel res -> ResolverMapT m b
withCache f resolver selection = do
  refs <- lift (scanRefs selection resolver)
  buildCache resolveRefs refs (f resolver selection)

withObject ::
  (MonadResolver m) =>
  Maybe TypeName ->
  (Maybe (SelectionSet VALID) -> m value) ->
  SelectionContent VALID ->
  m value
withObject __typename f = updateCurrentType __typename . checkContent
  where
    checkContent (SelectionSet selection) = f (Just selection)
    checkContent (UnionSelection interface unionSel) = do
      typename <- asks (typeName . currentType)
      selection <- selectOr (pure interface) (fx interface) typename unionSel
      f selection
      where
        fx (Just x) y = Just <$> (x <:> unionTagSelection y)
        fx Nothing y = pure $ Just $ unionTagSelection y
    checkContent SelectionField = noEmptySelection

noEmptySelection :: (MonadError GQLError m, MonadReader ResolverContext m) => m value
noEmptySelection = do
  sel <- asks currentSelection
  throwError $ subfieldsNotSelected (selectionName sel) "" (selectionPosition sel)

withSingle :: (MonadError GQLError f, Show a) => [a] -> f a
withSingle [x] = pure x
withSingle x = throwError (internal ("expected only one resolved value for " <> msg (show x :: String)))

-- RESOLVING
resolveRef :: (MonadResolver m) => SelectionRef -> ResolverMapT m ValidValue
resolveRef ref = resolveRefs ref >>= withSingle

resolveRefs :: (MonadResolver m) => SelectionRef -> ResolverMapT m [ValidValue]
resolveRefs ref = do
  (ks, cache) <- genCache resolveUncached ref
  traverse (useCached cache) ks

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

processResult ::
  (MonadResolver m) =>
  TypeName ->
  SelectionContent VALID ->
  NamedResolverResult m ->
  ResolverMapT m ValidValue
processResult typename selection (NamedObjectResolver res) = do
  ctx <- ask
  lift $ withObject (Just typename) (resolveObject ctx res) selection
processResult _ selection (NamedUnionResolver unionRef) = resolveSelection (ResRef $ pure unionRef) selection
processResult _ selection (NamedEnumResolver value) = resolveSelection (ResEnum value) selection
processResult _ selection NamedNullResolver = resolveSelection ResNull selection
processResult _ selection (NamedScalarResolver v) = resolveSelection (ResScalar v) selection

resolveUncached :: (MonadResolver m) => SelectionRef -> ResolverMapT m [ValidValue]
resolveUncached (_, NamedResolverRef _ []) = pure empty
resolveUncached (selection, NamedResolverRef {..}) = do
  rmap <- asks resolverMap
  results <- lift (selectOr notFound found resolverTypeName rmap)
  traverse (processResult resolverTypeName selection) results
  where
    found :: (MonadResolver m) => NamedResolver m -> m [NamedResolverResult m]
    found = (resolverArgument &) . resolverFun
    notFound :: (MonadResolver m) => m [NamedResolverResult m]
    notFound = throwError ("resolver type " <> msg resolverTypeName <> "can't found")

resolveObject ::
  (MonadResolver m) =>
  ResolverMapContext m ->
  ObjectTypeResolver m ->
  Maybe (SelectionSet VALID) ->
  m ValidValue
resolveObject =
  refreshCache (\drv sel -> Object <$> maybe (pure empty) (traverseCollection (resolverWithCache drv)) sel)

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

withField :: Monad m' => a -> (m (ResolverValue m) -> m' a) -> FieldName -> ObjectTypeResolver m -> m' a
withField fb suc selectionName ObjectTypeResolver {..} = maybe (pure fb) suc (lookup selectionName objectFields)

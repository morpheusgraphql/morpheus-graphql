{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
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
    buildCacheWith,
    runResMapT,
    splitCached,
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
    ResolverMap,
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

scanRefs :: (MonadResolver m) => SelectionContent VALID -> ResolverValue m -> m [SelectionRef]
scanRefs sel (ResList xs) = concat <$> traverse (scanRefs sel) xs
scanRefs sel (ResLazy x) = x >>= scanRefs sel
scanRefs sel (ResObject tyName obj) = withObject tyName (objectRefs obj) sel
scanRefs sel (ResRef ref) = pure . (sel,) <$> ref
scanRefs _ ResEnum {} = pure []
scanRefs _ ResNull = pure []
scanRefs _ ResScalar {} = pure []

objectRefs :: (MonadResolver m) => ObjectTypeResolver m -> Maybe (SelectionSet VALID) -> m [SelectionRef]
objectRefs _ Nothing = pure []
objectRefs dr (Just sel) = concat <$> traverse (fieldRefs dr) (toList sel)

fieldRefs :: (MonadResolver m) => ObjectTypeResolver m -> Selection VALID -> m [SelectionRef]
fieldRefs objRes currentSelection@Selection {..}
  | selectionName == "__typename" = pure []
  | otherwise = do
      t <- askFieldTypeName selectionName
      updateCurrentType t $
        local (\ctx -> ctx {currentSelection}) $ do
          x <- withField [] (fmap pure) selectionName objRes
          concat <$> traverse (scanRefs selectionContent) x

resolveSelection ::
  (MonadResolver m) =>
  ResolverValue m ->
  SelectionContent VALID ->
  ResolverMapT m ValidValue
resolveSelection res selection = do
  refs <- lift (scanRefs selection res)
  buildCache refs (__resolveSelection res selection)

buildCache :: (MonadResolver m) => [SelectionRef] -> ResolverMapT m a -> ResolverMapT m a
buildCache refs m = do
  ctx <- buildCacheWith resolveRefs refs
  local (const ctx) m

__resolveSelection ::
  (MonadResolver m) =>
  ResolverValue m ->
  SelectionContent VALID ->
  ResolverMapT m ValidValue
__resolveSelection (ResLazy x) selection = lift x >>= (`resolveSelection` selection)
__resolveSelection (ResList xs) selection = List <$> traverse (`resolveSelection` selection) xs
__resolveSelection (ResObject tyName obj) sel = do
  ctx <- ask
  lift $ withObject tyName (resolveObject ctx obj) sel
__resolveSelection (ResEnum name) SelectionField = pure $ Scalar $ String $ unpackName name
__resolveSelection (ResEnum name) unionSel@UnionSelection {} = resolveSelection (mkUnion name [(unitFieldName, pure $ mkEnum unitTypeName)]) unionSel
__resolveSelection ResEnum {} _ = throwError (internal "wrong selection on enum value")
__resolveSelection ResNull _ = pure Null
__resolveSelection (ResScalar x) SelectionField = pure $ Scalar x
__resolveSelection ResScalar {} _ = throwError (internal "scalar resolver should only receive SelectionField")
__resolveSelection (ResRef mRef) sel = do
  ref <- lift mRef
  resolveRef (sel, ref)

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

resolveRef :: (MonadResolver m) => SelectionRef -> ResolverMapT m ValidValue
resolveRef ref = resolveRefs ref >>= withSingle

withSingle :: (MonadError GQLError f, Show a) => [a] -> f a
withSingle [x] = pure x
withSingle x = throwError (internal ("expected only one resolved value for " <> msg (show x :: String)))

resolveRefs :: (MonadResolver m) => SelectionRef -> ResolverMapT m [ValidValue]
resolveRefs (selection, NamedResolverRef name args) = do
  ctx <- ask
  let (cachedMap, uncachedSelection) = splitCached ctx (selection, NamedResolverRef name args)
  uncachedMap <- resolveUncached name uncachedSelection
  traverse (useCached (cachedMap <> uncachedMap)) args

resolveUncached ::
  (MonadResolver m) =>
  TypeName ->
  (SelectionContent VALID, [ValidValue]) ->
  ResolverMapT m (HashMap ValidValue ValidValue)
resolveUncached _ (_, []) = pure empty
resolveUncached typename (selection, xs) = do
  resolver <- getNamedResolverBy (NamedResolverRef typename xs)
  values <- traverse (processResult typename selection) resolver
  pure $ unsafeFromList (zip xs values)

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

getNamedResolverBy ::
  (MonadResolver m) =>
  NamedResolverRef ->
  ResolverMapT m [NamedResolverResult m]
getNamedResolverBy NamedResolverRef {..} = do
  rmap <- asks resolverMap
  lift (selectOr notFound found resolverTypeName rmap)
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
resolveObject rmap drv sel =
  refreshCache
    rmap
    drv
    sel
    (Object <$> maybe (pure empty) (traverseCollection (resolverWithCache drv)) sel)

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
  ResolverMapContext m ->
  ObjectTypeResolver m ->
  Maybe (SelectionSet VALID) ->
  ResolverMapT m v ->
  m v
refreshCache ctx drv sel v
  -- TODO: this is workaround to fix https://github.com/morpheusgraphql/morpheus-graphql/issues/810
  -- which deactivates caching for non named resolvers. find out better long term solution
  | null (resolverMap ctx) = runResMapT v ctx
  | otherwise = do
      refs <- objectRefs drv sel
      runResMapT (buildCache refs v) ctx

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

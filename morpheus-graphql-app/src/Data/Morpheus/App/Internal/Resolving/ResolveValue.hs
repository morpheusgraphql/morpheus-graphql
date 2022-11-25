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
import qualified Data.HashMap.Lazy as HM
import Data.Morpheus.App.Internal.Resolving.Batching
  ( CacheKey (..),
    ResolverMapContext (..),
    ResolverMapT,
    buildCacheWith,
    runResMapT,
    useCached,
  )
import Data.Morpheus.App.Internal.Resolving.Resolver (MonadResolver)
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
  ( KeyOf (keyOf),
    empty,
    selectOr,
    traverseCollection,
    (<:>),
  )
import Data.Morpheus.Types.Internal.AST
  ( GQLError,
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

scanRefs :: (MonadResolver m) => SelectionContent VALID -> ResolverValue m -> m [(SelectionContent VALID, NamedResolverRef)]
scanRefs sel (ResList xs) = concat <$> traverse (scanRefs sel) xs
scanRefs sel (ResLazy x) = x >>= scanRefs sel
scanRefs sel (ResObject tyName obj) = withObject tyName (objectRefs obj) sel
scanRefs sel (ResRef ref) = pure . (sel,) <$> ref
scanRefs _ ResEnum {} = pure []
scanRefs _ ResNull = pure []
scanRefs _ ResScalar {} = pure []

objectRefs ::
  (MonadResolver m) =>
  ObjectTypeResolver m ->
  Maybe (SelectionSet VALID) ->
  m [(SelectionContent VALID, NamedResolverRef)]
objectRefs _ Nothing = pure []
objectRefs dr (Just sel) = concat <$> traverse (fieldRefs dr) (toList sel)

fieldRefs ::
  (MonadResolver m) =>
  ObjectTypeResolver m ->
  Selection VALID ->
  m [(SelectionContent VALID, NamedResolverRef)]
fieldRefs ObjectTypeResolver {..} currentSelection@Selection {..}
  | selectionName == "__typename" = pure []
  | otherwise = do
      t <- askFieldTypeName selectionName
      updateCurrentType t $
        local (\ctx -> ctx {currentSelection}) $ do
          x <- maybe (pure []) (fmap pure) (HM.lookup selectionName objectFields)
          concat <$> traverse (scanRefs selectionContent) x

resolveSelection ::
  (MonadResolver m) =>
  ResolverValue m ->
  SelectionContent VALID ->
  ResolverMapT m ValidValue
resolveSelection res selection = do
  ctx <- ask
  newRmap <- lift (scanRefs selection res >>= buildCache ctx)
  local (const newRmap) (__resolveSelection res selection)

buildCache :: (MonadResolver m) => ResolverMapContext m -> [(SelectionContent VALID, NamedResolverRef)] -> m (ResolverMapContext m)
buildCache ctx@(ResolverMapContext cache rmap) entries = (`ResolverMapContext` rmap) <$> buildCacheWith (resolveRefsCached ctx) cache entries

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
__resolveSelection ResScalar {} _ = throwError (internal "scalar Resolver should only receive SelectionField")
__resolveSelection (ResRef ref) sel = do
  ctx <- ask
  lift (ref >>= flip (resolveRef ctx) sel)

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

resolveRef ::
  (MonadResolver m) =>
  ResolverMapContext m ->
  NamedResolverRef ->
  SelectionContent VALID ->
  m ValidValue
resolveRef rmap ref selection = resolveRefsCached rmap ref selection >>= toOne

toOne :: (MonadError GQLError f, Show a) => [a] -> f a
toOne [x] = pure x
toOne x = throwError (internal ("expected only one resolved value for " <> msg (show x :: String)))

resolveRefsCached ::
  (MonadResolver m) =>
  ResolverMapContext m ->
  NamedResolverRef ->
  SelectionContent VALID ->
  m [ValidValue]
resolveRefsCached ctx (NamedResolverRef name args) selection = do
  let keys = map (CacheKey selection name) args
  let cached = map resolveCached keys
  let cachedMap = HM.fromList (mapMaybe unp cached)
  notCachedMap <- runResMapT (resolveUncached name selection $ map fst $ filter (isNothing . snd) cached) ctx
  traverse (useCached (cachedMap <> notCachedMap)) args
  where
    unp (_, Nothing) = Nothing
    unp (x, Just y) = Just (x, y)
    resolveCached key = (cachedArg key, HM.lookup key $ localCache ctx)

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

resolveUncached ::
  (MonadResolver m) =>
  TypeName ->
  SelectionContent VALID ->
  [ValidValue] ->
  ResolverMapT m (HashMap ValidValue ValidValue)
resolveUncached _ _ [] = pure empty
resolveUncached typename selection xs = do
  rmap <- asks resolverMap
  vs <- lift (getNamedResolverBy (NamedResolverRef typename xs) rmap) >>= traverse (processResult typename selection)
  pure $ HM.fromList (zip xs vs)

getNamedResolverBy ::
  (MonadResolver m) =>
  NamedResolverRef ->
  ResolverMap m ->
  m [NamedResolverResult m]
getNamedResolverBy NamedResolverRef {..} = selectOr cantFoundError ((resolverArgument &) . resolverFun) resolverTypeName
  where
    cantFoundError = throwError ("Resolver Type " <> msg resolverTypeName <> "can't found")

resolveObject ::
  (MonadResolver m) =>
  ResolverMapContext m ->
  ObjectTypeResolver m ->
  Maybe (SelectionSet VALID) ->
  m ValidValue
resolveObject rmap drv sel = do
  newCache <- objectRefs drv sel >>= buildCache rmap
  Object <$> maybe (pure empty) (traverseCollection (resolver newCache)) sel
  where
    resolver cacheCTX currentSelection = do
      t <- askFieldTypeName (selectionName currentSelection)
      updateCurrentType t $
        local (\ctx -> ctx {currentSelection}) $
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
  | otherwise =
      maybe (pure Null) (lift >=> (`resolveSelection` selectionContent))
        . HM.lookup selectionName
        . objectFields

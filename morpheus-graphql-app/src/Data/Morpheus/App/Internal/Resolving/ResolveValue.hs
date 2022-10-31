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
  )
where

import Control.Monad.Except (MonadError (throwError))
import qualified Data.HashMap.Lazy as HM
import Data.Morpheus.App.Internal.Resolving.ResolverState
  ( ResolverContext (..),
    askFieldTypeName,
    updateCurrentType,
  )
import Data.Morpheus.App.Internal.Resolving.Types
  ( BatchEntry (..),
    CacheKey (..),
    LocalCache,
    NamedResolver (..),
    NamedResolverRef (..),
    NamedResolverResult (..),
    ObjectTypeResolver (..),
    ResolverMap,
    ResolverValue (..),
    buildBatches,
    dumpCache,
    mkEnum,
    mkUnion,
    useCached,
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

scanRefs :: (MonadError GQLError m, MonadReader ResolverContext m) => SelectionContent VALID -> ResolverValue m -> m [(SelectionContent VALID, NamedResolverRef)]
scanRefs sel (ResList xs) = concat <$> traverse (scanRefs sel) xs
scanRefs sel (ResLazy x) = x >>= scanRefs sel
scanRefs sel (ResObject tyName obj) = withObject tyName (objectRefs obj) sel
scanRefs sel (ResRef ref) = pure . (sel,) <$> ref
scanRefs _ ResEnum {} = pure []
scanRefs _ ResNull = pure []
scanRefs _ ResScalar {} = pure []

objectRefs ::
  ( MonadError GQLError m,
    MonadReader ResolverContext m
  ) =>
  ObjectTypeResolver m ->
  Maybe (SelectionSet VALID) ->
  m [(SelectionContent VALID, NamedResolverRef)]
objectRefs _ Nothing = pure []
objectRefs dr (Just sel) = concat <$> traverse (fieldRefs dr) (toList sel)

fieldRefs ::
  (MonadError GQLError m, MonadReader ResolverContext m) =>
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
  ( Monad m,
    MonadReader ResolverContext m,
    MonadError GQLError m
  ) =>
  (LocalCache, ResolverMap m) ->
  ResolverValue m ->
  SelectionContent VALID ->
  m ValidValue
resolveSelection rmap res selection = do
  newRmap <- scanRefs selection res >>= buildCache rmap . buildBatches
  __resolveSelection newRmap res selection

buildCache :: (MonadError GQLError m, MonadReader ResolverContext m) => (LocalCache, ResolverMap m) -> [BatchEntry] -> m (LocalCache, ResolverMap m)
buildCache (cache, rmap) entries = do
  caches <- traverse (resolveCacheEntry (cache, rmap)) entries
  let newCache = foldr (<>) cache caches
  pure $ dumpCache False (newCache, rmap)

resolveCacheEntry :: (MonadError GQLError m, MonadReader ResolverContext m) => (LocalCache, ResolverMap m) -> BatchEntry -> m LocalCache
resolveCacheEntry rmap (BatchEntry sel name deps) = do
  res <- resolveRefsCached rmap (NamedResolverRef name deps) sel
  let keys = map (CacheKey sel name) deps
  let entries = zip keys res
  pure $ HM.fromList entries

__resolveSelection ::
  ( Monad m,
    MonadReader ResolverContext m,
    MonadError GQLError m
  ) =>
  (LocalCache, ResolverMap m) ->
  ResolverValue m ->
  SelectionContent VALID ->
  m ValidValue
__resolveSelection rmap (ResLazy x) selection =
  x >>= flip (resolveSelection rmap) selection
__resolveSelection rmap (ResList xs) selection =
  List <$> traverse (flip (resolveSelection rmap) selection) xs
-- Object -----------------
__resolveSelection rmap (ResObject tyName obj) sel = withObject tyName (resolveObject rmap obj) sel
-- ENUM
__resolveSelection _ (ResEnum name) SelectionField = pure $ Scalar $ String $ unpackName name
__resolveSelection rmap (ResEnum name) unionSel@UnionSelection {} =
  resolveSelection rmap (mkUnion name [(unitFieldName, pure $ mkEnum unitTypeName)]) unionSel
__resolveSelection _ ResEnum {} _ = throwError (internal "wrong selection on enum value")
-- SCALARS
__resolveSelection _rmap ResNull _ = pure Null
__resolveSelection _rmap (ResScalar x) SelectionField = pure $ Scalar x
__resolveSelection _rmap ResScalar {} _ =
  throwError (internal "scalar Resolver should only receive SelectionField")
__resolveSelection rmap (ResRef ref) sel = ref >>= flip (resolveRef rmap) sel

withObject ::
  ( MonadError GQLError m,
    MonadReader ResolverContext m
  ) =>
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
    checkContent _ = noEmptySelection

noEmptySelection :: (MonadError GQLError m, MonadReader ResolverContext m) => m value
noEmptySelection = do
  sel <- asks currentSelection
  throwError $ subfieldsNotSelected (selectionName sel) "" (selectionPosition sel)

resolveRef ::
  ( MonadError GQLError m,
    MonadReader ResolverContext m
  ) =>
  (LocalCache, ResolverMap m) ->
  NamedResolverRef ->
  SelectionContent VALID ->
  m ValidValue
resolveRef rmap ref selection = resolveRefsCached rmap ref selection >>= toOne

toOne :: (MonadError GQLError f) => [a] -> f a
toOne [x] = pure x
toOne _ = throwError (internal "TODO:")

resolveRefsCached ::
  ( MonadError GQLError m,
    MonadReader ResolverContext m
  ) =>
  (LocalCache, ResolverMap m) ->
  NamedResolverRef ->
  SelectionContent VALID ->
  m [ValidValue]
resolveRefsCached (cache, rmap) (NamedResolverRef name args) selection = do
  let keys = map (CacheKey selection name) args
  let cached = map resolveCached keys
  let cachedMap = HM.fromList (mapMaybe unp cached)
  notCachedMap <- resolveUncached (cache, rmap) name selection $ map fst $ filter (isNothing . snd) cached
  traverse (useCached (cachedMap <> notCachedMap)) args
  where
    unp (_, Nothing) = Nothing
    unp (x, Just y) = Just (x, y)
    resolveCached key = (cachedArg key, HM.lookup key cache)

processResult ::
  (MonadError GQLError m, MonadReader ResolverContext m) =>
  (LocalCache, ResolverMap m) ->
  TypeName ->
  SelectionContent VALID ->
  NamedResolverResult m ->
  m ValidValue
processResult rmap typename selection (NamedObjectResolver res) = withObject (Just typename) (resolveObject rmap res) selection
processResult rmap _ selection (NamedUnionResolver unionRef) = resolveSelection rmap (ResRef $ pure unionRef) selection
processResult rmap _ selection (NamedEnumResolver value) = resolveSelection rmap (ResEnum value) selection

resolveUncached ::
  ( MonadError GQLError m,
    MonadReader ResolverContext m
  ) =>
  (LocalCache, ResolverMap m) ->
  TypeName ->
  SelectionContent VALID ->
  [ValidValue] ->
  m (HashMap ValidValue ValidValue)
resolveUncached _ _ _ [] = pure empty
resolveUncached rmap typename selection xs = do
  vs <- getNamedResolverBy (NamedResolverRef typename xs) (snd rmap) >>= traverse (processResult rmap typename selection)
  pure $ HM.fromList (zip xs vs)

getNamedResolverBy ::
  (MonadError GQLError m) =>
  NamedResolverRef ->
  ResolverMap m ->
  m [NamedResolverResult m]
getNamedResolverBy NamedResolverRef {..} = selectOr cantFoundError ((resolverArgument &) . resolverFun) resolverTypeName
  where
    cantFoundError = throwError ("Resolver Type " <> msg resolverTypeName <> "can't found")

resolveObject ::
  ( MonadReader ResolverContext m,
    MonadError GQLError m
  ) =>
  (LocalCache, ResolverMap m) ->
  ObjectTypeResolver m ->
  Maybe (SelectionSet VALID) ->
  m ValidValue
resolveObject rmap drv sel = do
  newCache <- objectRefs drv sel >>= buildCache rmap . buildBatches
  Object <$> maybe (pure empty) (traverseCollection (resolver newCache)) sel
  where
    resolver newCache currentSelection = do
      t <- askFieldTypeName (selectionName currentSelection)
      updateCurrentType t $
        local (\ctx -> ctx {currentSelection}) $
          ObjectEntry (keyOf currentSelection)
            <$> runFieldResolver newCache currentSelection drv

runFieldResolver ::
  ( Monad m,
    MonadReader ResolverContext m,
    MonadError GQLError m
  ) =>
  (LocalCache, ResolverMap m) ->
  Selection VALID ->
  ObjectTypeResolver m ->
  m ValidValue
runFieldResolver rmap Selection {selectionName, selectionContent}
  | selectionName == "__typename" =
    const (Scalar . String . unpackName <$> asks (typeName . currentType))
  | otherwise =
    maybe (pure Null) (>>= \x -> resolveSelection rmap x selectionContent)
      . HM.lookup selectionName
      . objectFields

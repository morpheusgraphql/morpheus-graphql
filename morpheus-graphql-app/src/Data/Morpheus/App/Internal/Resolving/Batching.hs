{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.App.Internal.Resolving.Batching
  ( CacheKey,
    LocalCache,
    ResolverMapContext (..),
    ResolverMapT (..),
    runResMapT,
    SelectionRef,
    withBatching,
  )
where

import Control.Monad.Except (MonadError (throwError))
import Data.ByteString.Lazy.Char8 (unpack)
import Data.HashMap.Lazy (keys)
import Data.Morpheus.App.Internal.Resolving.Cache (CacheKey (CacheKey, cachedArg), CacheStore, initCache, isNotCached, mergeCache, printSelectionKey, useCached)
import Data.Morpheus.App.Internal.Resolving.Refs (scanRefs)
import Data.Morpheus.App.Internal.Resolving.ResolverState (ResolverContext, config)
import Data.Morpheus.App.Internal.Resolving.Types
  ( NamedResolver (..),
    NamedResolverResult (..),
    ResolverMap,
  )
import Data.Morpheus.App.Internal.Resolving.Utils
import Data.Morpheus.Core (Config (..), render)
import Data.Morpheus.Internal.Utils (IsMap (..), selectOr)
import Data.Morpheus.Types.Internal.AST
  ( GQLError,
    Msg (..),
    SelectionContent,
    TypeName,
    VALID,
    ValidValue,
    internal,
  )
import Debug.Trace (trace)
import GHC.Show (Show (show))
import Relude hiding (empty, show, trace)

type LocalCache = CacheStore

dumpCache :: Bool -> LocalCache -> LocalCache
dumpCache enabled cache
  | not enabled = cache
  | otherwise = trace ("\nCACHE:\n" <> show cache) cache

data BatchEntry = BatchEntry
  { batchedSelection :: SelectionContent VALID,
    batchedType :: TypeName,
    batchedArguments :: [ValidValue]
  }

instance Show BatchEntry where
  show BatchEntry {..} = printSelectionKey batchedSelection <> ":" <> toString batchedType <> ":" <> show (map (unpack . render) batchedArguments)

type SelectionRef = (SelectionContent VALID, NamedResolverRef)

uniq :: (Eq a, Hashable a) => [a] -> [a]
uniq = keys . unsafeFromList . map (,True)

buildBatches :: [SelectionRef] -> [BatchEntry]
buildBatches inputs =
  let entityTypes = uniq $ map (second resolverTypeName) inputs
   in mapMaybe (selectByEntity inputs) entityTypes

selectByEntity :: [SelectionRef] -> (SelectionContent VALID, TypeName) -> Maybe BatchEntry
selectByEntity inputs (tSel, tName) = case filter areEq inputs of
  [] -> Nothing
  xs -> Just $ BatchEntry tSel tName (uniq $ concatMap (resolverArgument . snd) xs)
  where
    areEq (sel, v) = sel == tSel && tName == resolverTypeName v

type ResolverFun m = SelectionRef -> m [ValidValue]

resolveBatched :: Monad m => ResolverFun m -> BatchEntry -> m LocalCache
resolveBatched f (BatchEntry sel name deps) = do
  res <- f (sel, NamedResolverRef name deps)
  let ks = map (CacheKey sel name) deps
  initCache (zip ks res)

data ResolverMapContext m = NamedContext
  { localCache :: LocalCache,
    resolverMap :: ResolverMap m
  }

newtype ResolverMapT m a = ResolverMapT
  { _runResMapT :: ReaderT (ResolverMapContext m) m a
  }
  deriving
    ( Functor,
      Applicative,
      Monad
    )

instance (MonadReader ResolverContext m) => MonadReader ResolverContext (ResolverMapT m) where
  ask = ResolverMapT (lift ask)
  local f (ResolverMapT m) = ResolverMapT (ReaderT (local f . runReaderT m))

instance MonadTrans ResolverMapT where
  lift = ResolverMapT . lift

deriving instance MonadError GQLError m => MonadError GQLError (ResolverMapT m)

setCache :: Monad m => LocalCache -> ResolverMapT m a -> ResolverMapT m a
setCache cache m = do
  ResolverMapT $ do
    rmap <- asks resolverMap
    local (const (NamedContext cache rmap)) (_runResMapT m)

getBatchingState :: Monad m => ResolverMapT m (ResolverMapContext m)
getBatchingState = ResolverMapT ask

runResMapT :: ResolverMapT m a -> ResolverMapContext m -> m a
runResMapT (ResolverMapT x) = runReaderT x

withSingle :: (MonadError GQLError f, Show a) => [a] -> f a
withSingle [x] = pure x
withSingle x = throwError (internal ("expected only one resolved value for " <> msg (show x :: String)))

cacheRefs :: (MonadError GQLError m) => ResolverFun (ResolverMapT m) -> SelectionRef -> ResolverMapT m [ValidValue]
cacheRefs f (selection, NamedResolverRef name args) = do
  ctx <- getBatchingState
  let ks = map (CacheKey selection name) args
  let uncached = map cachedArg $ filter (isNotCached (localCache ctx)) ks
  let batches = buildBatches [(selection, NamedResolverRef name uncached)]
  caches <- traverse (resolveBatched f) batches
  let cache = mergeCache (localCache ctx) caches
  traverse (useCached cache) ks

type SelectionResolverFun m = SelectionContent VALID -> ResolverValue m -> ResolverMapT m ValidValue

updateCache :: (ResolverMonad m) => ResolverFun (ResolverMapT m) -> [BatchEntry] -> ResolverMapT m LocalCache
updateCache f entries = do
  ctx <- getBatchingState
  caches <- traverse (resolveBatched f) entries
  let newCache = mergeCache (localCache ctx) caches
  enabled <- asks (debug . config)
  pure $ dumpCache enabled newCache

cachedWith ::
  (ResolverMonad m) =>
  (SelectionRef -> ResolverMapT m [ValidValue]) ->
  SelectionResolverFun m ->
  SelectionContent VALID ->
  ResolverValue m ->
  ResolverMapT m ValidValue
cachedWith resolveRef resolveSelection selection resolver = do
  refs <- lift (scanRefs selection resolver)
  newCache <- updateCache (cacheRefs resolveRef) (buildBatches refs)
  setCache newCache (resolveSelection selection resolver)

-- RESOLVING
withBatching :: ResolverMonad m => SelectionResolverFun m -> SelectionRef -> ResolverMapT m ValidValue
withBatching resolve = cacheRefs resolveRef >=> withSingle
  where
    resolveRef (selection, ref) = do
      values <- runNamedResolverRef ref
      traverse (cachedWith resolveRef resolve selection) values

runNamedResolverRef :: (MonadError GQLError m, MonadReader ResolverContext m) => NamedResolverRef -> ResolverMapT m [ResolverValue m]
runNamedResolverRef NamedResolverRef {..}
  | null resolverArgument = pure []
  | otherwise = do
    resolvers <- ResolverMapT (asks resolverMap)
    NamedResolver {resolverFun} <- lift (selectOr notFound pure resolverTypeName resolvers)
    map (toResolverValue resolverTypeName) <$> lift (resolverFun resolverArgument)
  where
    notFound = throwError ("resolver type " <> msg resolverTypeName <> "can't found")

toResolverValue :: (Monad m) => TypeName -> NamedResolverResult m -> ResolverValue m
toResolverValue typeName (NamedObjectResolver res) = ResObject (Just typeName) res
toResolverValue _ (NamedUnionResolver unionRef) = ResRef $ pure unionRef
toResolverValue _ (NamedEnumResolver value) = ResEnum value
toResolverValue _ NamedNullResolver = ResNull
toResolverValue _ (NamedScalarResolver v) = ResScalar v

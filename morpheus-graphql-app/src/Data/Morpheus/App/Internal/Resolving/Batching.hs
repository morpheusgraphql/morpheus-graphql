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
import Data.Morpheus.App.Internal.Resolving.Cache
  ( CacheKey (CacheKey, cachedArg),
    CacheStore,
    initCache,
    isNotCached,
    mergeCache,
    printSelectionKey,
    useCached,
    withDebug,
  )
import Data.Morpheus.App.Internal.Resolving.Refs (scanRefs)
import Data.Morpheus.App.Internal.Resolving.ResolverState (ResolverContext)
import Data.Morpheus.App.Internal.Resolving.Types
  ( NamedResolver (..),
    NamedResolverResult (..),
    ResolverMap,
  )
import Data.Morpheus.App.Internal.Resolving.Utils
import Data.Morpheus.Core (render)
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
import GHC.Show (Show (show))
import Relude hiding (empty, show)

type LocalCache = CacheStore

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

toCacheKey :: SelectionContent VALID -> TypeName -> [ValidValue] -> [CacheKey]
toCacheKey sel name = map (CacheKey sel name)

zipBatched :: Monad m => BatchEntry -> [ValidValue] -> m CacheStore
zipBatched (BatchEntry sel name deps) res = do
  let cacheKeys = toCacheKey sel name deps
  initCache (zip cacheKeys res)

batchesToCache :: MonadReader ResolverContext m => ResolverFun (ResolverMapT m) -> [SelectionRef] -> ResolverMapT m CacheStore
batchesToCache f refs = do
  oldCache <- getCached
  caches <- traverse (resolveBatched f) (buildBatches refs)
  withDebug (mergeCache oldCache caches)

resolveBatched :: Monad m => ResolverFun m -> BatchEntry -> m LocalCache
resolveBatched f b@(BatchEntry sel name deps) = f (sel, NamedResolverRef name deps) >>= zipBatched b

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

getCached :: Monad m => ResolverMapT m CacheStore
getCached = ResolverMapT (asks localCache)

runResMapT :: ResolverMapT m a -> ResolverMapContext m -> m a
runResMapT (ResolverMapT x) = runReaderT x

withSingle :: (MonadError GQLError f, Show a) => [a] -> f a
withSingle [x] = pure x
withSingle x = throwError (internal ("expected only one resolved value for " <> msg (show x :: String)))

type SelectionResolverFun m = SelectionContent VALID -> ResolverValue m -> ResolverMapT m ValidValue

-- RESOLVING
withBatching :: ResolverMonad m => SelectionResolverFun m -> SelectionRef -> ResolverMapT m ValidValue
withBatching resolveSelection = resolveRefsWitchCaching >=> withSingle
  where
    resolveRefsWitchCaching (selection, NamedResolverRef name args) = do
      oldCache <- getCached
      let cacheKeys = toCacheKey selection name args
      let uncached = map cachedArg $ filter (isNotCached oldCache) cacheKeys
      cache <- batchesToCache resolveUncachedRefs [(selection, NamedResolverRef name uncached)]
      traverse (useCached cache) cacheKeys
      where
        resolveUncachedRefs (s, ref) = runNamedResolverRef ref >>= traverse resolveValue
          where
            resolveValue value = do
              refs <- lift (scanRefs s value)
              cache <- batchesToCache resolveRefsWitchCaching refs
              setCache cache (resolveSelection s value)

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

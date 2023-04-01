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
    NamedContext (..),
    ResolverMapT (..),
    runResMapT,
    SelectionRef,
    withBatching,
  )
where

import Control.Monad.Except (MonadError (throwError))
import Data.ByteString.Lazy.Char8 (unpack)
import Data.HashMap.Lazy (keys, traverseWithKey)
import qualified Data.HashMap.Lazy as HM
import Data.Morpheus.App.Internal.Resolving.Cache
  ( CacheKey (..),
    CacheStore (CacheStore),
    CacheValue (..),
    insertPres,
    mergeCache,
    printSelectionKey,
    toUncached,
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
  ( NamedResolverRef (..),
    ResolverMonad,
    ResolverValue (ResEnum, ResNull, ResObject, ResRef, ResScalar),
  )
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
import Relude hiding (show)

type LocalCache = CacheStore

data BatchEntry = BatchEntry
  { batchedSelection :: SelectionContent VALID,
    batchedType :: TypeName,
    batchedArguments :: [ValidValue]
  }

instance Show BatchEntry where
  show BatchEntry {..} =
    "\nBATCH(" <> toString batchedType <> "):"
      <> "\n  sel:"
      <> printSelectionKey batchedSelection
      <> "\n  dep:"
      <> show (map (unpack . render) batchedArguments)

type SelectionRef = (SelectionContent VALID, NamedResolverRef)

uniq :: (Eq a, Hashable a) => [a] -> [a]
uniq = keys . unsafeFromList . map (,True)

buildBatches :: [SelectionRef] -> [BatchEntry]
buildBatches inputs =
  let entityTypes = uniq $ map (second resolverTypeName) inputs
   in mapMaybe (selectByEntity inputs) entityTypes

selectByEntity :: [SelectionRef] -> (SelectionContent VALID, TypeName) -> Maybe BatchEntry
selectByEntity inputs (tSel, tName) = case gerArgs (filter areEq inputs) of
  [] -> Nothing
  args -> Just (BatchEntry tSel tName args)
    where
  where
    gerArgs = uniq . concatMap (resolverArgument . snd)
    areEq (sel, v) = sel == tSel && tName == resolverTypeName v

data NamedContext m = NamedContext
  { localCache :: LocalCache m,
    resolverMap :: ResolverMap m
  }

newtype ResolverMapT m a = ResolverMapT
  { _runResMapT :: ReaderT (NamedContext m) m a
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

getCached :: Monad m => ResolverMapT m (CacheStore m)
getCached = ResolverMapT (asks localCache)

runResMapT :: ResolverMapT m a -> NamedContext m -> m a
runResMapT (ResolverMapT x) = runReaderT x

withSingle :: (MonadError GQLError f, Show a) => [a] -> f a
withSingle [x] = pure x
withSingle x = throwError (internal ("expected only one resolved value for " <> msg (show x :: String)))

type SelectionResolverFun m = SelectionContent VALID -> ResolverValue m -> ResolverMapT m ValidValue

mkBatch :: SelectionRef -> BatchEntry
mkBatch (sel, NamedResolverRef name args) = BatchEntry sel name (uniq args)

fullCache :: Monad m => [CacheStore m] -> ResolverMapT m (CacheStore m)
fullCache caches = (`mergeCache` caches) <$> ResolverMapT (asks localCache)

toCacheKey :: SelectionContent VALID -> TypeName -> [ValidValue] -> [CacheKey]
toCacheKey sel name = map (CacheKey sel name)

setPres :: Monad m => Prefetches m -> ResolverMapT m a -> ResolverMapT m a
setPres ps = ResolverMapT . local update . _runResMapT
  where
    update x = x {localCache = insertPres (localCache x) (HM.toList ps)}

type Prefetches m = HashMap CacheKey (ResolverValue m)

zipPrefetches :: Monad m => BatchEntry -> [ResolverValue m] -> Prefetches m
zipPrefetches (BatchEntry sel name deps) res = do
  let cacheKeys = toCacheKey sel name deps
  unsafeFromList (zip cacheKeys res)

-- RESOLVING
withBatching :: ResolverMonad m => SelectionResolverFun m -> SelectionRef -> ResolverMapT m ValidValue
withBatching resolveSelection ref = do
  oldCache <- getCached
  let (cacheKeys, uncached) = second (fmap mkBatch) (toUncached oldCache ref)
  pres <- maybe (pure mempty) prefetch uncached
  setPres pres $ do
    cache <- traverseWithKey (resolveSelection . cachedSel) pres >>= (fullCache . pure . CacheStore) . fmap CachedValue >>= withDebug
    x <- traverse (useCached cache) cacheKeys >>= withSingle
    case x of
      (CachedValue v) -> pure v
      (CachedResolver v) -> resolveSelection (fst ref) v

prefetch :: ResolverMonad m => BatchEntry -> ResolverMapT m (Prefetches m)
prefetch batch = do
  value <- run batch
  batches <- buildBatches . concat <$> traverse (lift . scanRefs (batchedSelection batch)) value
  xs <- traverse (\b -> zipPrefetches b <$> run b) batches
  let pre = fold (zipPrefetches batch value : xs)
  pure pre
  where
    run = withDebug >=> runBatch

runBatch :: (MonadError GQLError m, MonadReader ResolverContext m) => BatchEntry -> ResolverMapT m [ResolverValue m]
runBatch (BatchEntry _ name deps)
  | null deps = pure []
  | otherwise = do
    resolvers <- ResolverMapT (asks resolverMap)
    NamedResolver {resolverFun} <- lift (selectOr notFound pure name resolvers)
    map (toResolverValue name) <$> lift (resolverFun deps)
  where
    notFound = throwError ("resolver type " <> msg name <> "can't found")

toResolverValue :: (Monad m) => TypeName -> NamedResolverResult m -> ResolverValue m
toResolverValue typeName (NamedObjectResolver res) = ResObject (Just typeName) res
toResolverValue _ (NamedUnionResolver unionRef) = ResRef $ pure unionRef
toResolverValue _ (NamedEnumResolver value) = ResEnum value
toResolverValue _ NamedNullResolver = ResNull
toResolverValue _ (NamedScalarResolver v) = ResScalar v

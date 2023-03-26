{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
  ( CacheKey (..),
    LocalCache,
    ResolverMapContext (..),
    ResolverMapT (..),
    runResMapT,
    SelectionRef,
    getBatchingState,
    lookupResolvers,
    withBatching,
  )
where

import Control.Monad.Except (MonadError (throwError))
import Data.ByteString.Lazy.Char8 (unpack)
import Data.HashMap.Lazy (keys)
import Data.Morpheus.App.Internal.Resolving.Refs (scanRefs)
import Data.Morpheus.App.Internal.Resolving.ResolverState (ResolverContext, config)
import Data.Morpheus.App.Internal.Resolving.Types
  ( NamedResolver (..),
    NamedResolverResult (..),
    ResolverMap,
  )
import Data.Morpheus.App.Internal.Resolving.Utils
import Data.Morpheus.Core (Config (..), RenderGQL, render)
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

type LocalCache = HashMap CacheKey ValidValue

useCached :: (IsMap k (m k), Eq k, Show k, Hashable k, MonadError GQLError f) => m k a -> k -> f a
useCached mp v = case lookup v mp of
  Just x -> pure x
  Nothing -> throwError (internal $ "cache value could not found for key" <> msg (show v :: String))

dumpCache :: Bool -> LocalCache -> LocalCache
dumpCache enabled cache
  | null cache || not enabled = cache
  | otherwise = trace ("\nCACHE:\n" <> printCache cache) cache

printCache :: LocalCache -> [Char]
printCache cache = intercalate "\n" (map printKeyValue $ toAssoc cache) <> "\n"
  where
    printKeyValue (key, v) = " " <> show key <> ": " <> unpack (render v)

printSel :: RenderGQL a => a -> [Char]
printSel sel = map replace $ filter ignoreSpaces $ unpack (render sel)
  where
    ignoreSpaces x = x /= ' '
    replace '\n' = ' '
    replace x = x

data BatchEntry = BatchEntry
  { batchedSelection :: SelectionContent VALID,
    batchedType :: TypeName,
    batchedArguments :: [ValidValue]
  }

instance Show BatchEntry where
  show BatchEntry {..} = printSel batchedSelection <> ":" <> toString batchedType <> ":" <> show (map (unpack . render) batchedArguments)

data CacheKey = CacheKey
  { cachedSel :: SelectionContent VALID,
    cachedTypeName :: TypeName,
    cachedArg :: ValidValue
  }
  deriving (Eq, Generic)

type SelectionRef = (SelectionContent VALID, NamedResolverRef)

instance Show CacheKey where
  show (CacheKey sel typename dep) = printSel sel <> ":" <> toString typename <> ":" <> unpack (render dep)

instance Hashable CacheKey where
  hashWithSalt s (CacheKey sel tyName arg) = hashWithSalt s (sel, tyName, render arg)

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
  let entries = zip ks res
  pure $ unsafeFromList entries

updateCache :: (ResolverMonad m, Traversable t) => ResolverFun m -> LocalCache -> t BatchEntry -> m LocalCache
updateCache f cache entries = do
  caches <- traverse (resolveBatched f) entries
  let newCache = foldr (<>) cache caches
  enabled <- asks (debug . config)
  pure $ dumpCache enabled newCache

data ResolverMapContext m = ResolverMapContext
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
    local (const (ResolverMapContext cache rmap)) (_runResMapT m)

lookupResolvers :: ResolverMonad m => NamedResolverRef -> ResolverMapT m [NamedResolverResult m]
lookupResolvers NamedResolverRef {..} = do
  rmap <- namedResolvers
  lift (selectOr notFound found resolverTypeName rmap)
  where
    found = (resolverArgument &) . resolverFun
    notFound = throwError ("resolver type " <> msg resolverTypeName <> "can't found")

namedResolvers :: Monad m => ResolverMapT m (ResolverMap m)
namedResolvers = resolverMap <$> ResolverMapT ask

getBatchingState :: Monad m => ResolverMapT m (ResolverMapContext m)
getBatchingState = ResolverMapT ask

runResMapT :: ResolverMapT m a -> ResolverMapContext m -> m a
runResMapT (ResolverMapT x) = runReaderT x

isNotCached :: ResolverMapContext m -> CacheKey -> Bool
isNotCached ctx key = isNothing $ lookup key $ localCache ctx

withSingle :: (MonadError GQLError f, Show a) => [a] -> f a
withSingle [x] = pure x
withSingle x = throwError (internal ("expected only one resolved value for " <> msg (show x :: String)))

cacheRefs :: (MonadError GQLError m) => ResolverFun (ResolverMapT m) -> SelectionRef -> ResolverMapT m [ValidValue]
cacheRefs f (selection, NamedResolverRef name args) = do
  ctx <- getBatchingState
  let ks = map (CacheKey selection name) args
  let uncached = map cachedArg $ filter (isNotCached ctx) ks
  let batches = buildBatches [(selection, NamedResolverRef name uncached)]
  caches <- traverse (resolveBatched f) batches
  let cache = fold (localCache ctx : caches)
  traverse (useCached cache) ks

cachedWith ::
  (ResolverMonad m) =>
  (SelectionRef -> ResolverMapT m [ValidValue]) ->
  (ResolverValue m -> SelectionContent VALID -> ResolverMapT m b) ->
  ResolverValue m ->
  SelectionContent VALID ->
  ResolverMapT m b
cachedWith resolveRef resolveValue resolver selection = do
  ctx <- getBatchingState
  refs <- lift (scanRefs selection resolver)
  newCache <- lift (updateCache (\x -> runResMapT (cacheRefs resolveRef x) ctx) (localCache ctx) (buildBatches refs))
  setCache newCache (resolveValue resolver selection)

-- RESOLVING
withBatching ::
  ResolverMonad m =>
  (SelectionContent VALID -> ResolverValue m -> ResolverMapT m ValidValue) ->
  SelectionContent VALID ->
  NamedResolverRef ->
  ResolverMapT m ValidValue
withBatching resolve sel namedRef = cacheRefs resolveNamed (sel, namedRef) >>= withSingle
  where
    resolveNamed ref = unpackNamedRef ref >>= traverse (\drv -> cachedWith resolveNamed (flip resolve) drv (fst ref))

unpackNamedRef (_, NamedResolverRef _ []) = pure []
unpackNamedRef (selection, ref) = do
  resolvers <- lookupResolvers ref
  pure $ map (toResolverValue (resolverTypeName ref)) resolvers

toResolverValue :: (Monad m) => TypeName -> NamedResolverResult m -> ResolverValue m
toResolverValue typeName (NamedObjectResolver res) = ResObject (Just typeName) res
toResolverValue _ (NamedUnionResolver unionRef) = ResRef $ pure unionRef
toResolverValue _ (NamedEnumResolver value) = ResEnum value
toResolverValue _ NamedNullResolver = ResNull
toResolverValue _ (NamedScalarResolver v) = ResScalar v

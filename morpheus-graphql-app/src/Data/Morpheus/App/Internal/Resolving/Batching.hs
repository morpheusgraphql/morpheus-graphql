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
    useCached,
    buildCache,
    ResolverMapContext (..),
    ResolverMapT (..),
    runResMapT,
    SelectionRef,
    splitCached,
  )
where

import Control.Monad.Except (MonadError (throwError))
import Data.ByteString.Lazy.Char8 (unpack)
import Data.HashMap.Lazy (keys)
import Data.Morpheus.App.Internal.Resolving.ResolverState (config)
import Data.Morpheus.App.Internal.Resolving.Types (ResolverMap)
import Data.Morpheus.App.Internal.Resolving.Utils
import Data.Morpheus.Core (Config (..), RenderGQL, render)
import Data.Morpheus.Internal.Utils
  ( IsMap (..),
  )
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
import Relude hiding (show, trace)

type LocalCache = HashMap CacheKey ValidValue

useCached :: (Eq k, Show k, Hashable k, MonadError GQLError f) => HashMap k a -> k -> f a
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

buildCache :: (ResolverMonad m) => ResolverFun (ResolverMapT m) -> [SelectionRef] -> ResolverMapT m a -> ResolverMapT m a
buildCache f refs m = do
  oldCtx@(ResolverMapContext cache rmap) <- ask
  ctx <- (`ResolverMapContext` rmap) <$> lift (updateCache (\x -> runResMapT (f x) oldCtx) cache (buildBatches refs))
  local (const ctx) m

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
      Monad,
      MonadReader (ResolverMapContext m)
    )

instance MonadTrans ResolverMapT where
  lift = ResolverMapT . lift

deriving instance MonadError GQLError m => MonadError GQLError (ResolverMapT m)

runResMapT :: ResolverMapT m a -> ResolverMapContext m -> m a
runResMapT (ResolverMapT x) = runReaderT x

splitCached :: Monad m => SelectionRef -> ResolverMapT m (HashMap ValidValue ValidValue, NamedResolverRef)
splitCached (selection, NamedResolverRef name args) = do
  ctx <- ask
  let ks = map (CacheKey selection name) args
  let cached = map (resolveCached ctx) ks
  let uncached = map fst $ filter (isNothing . snd) cached
  pure (unsafeFromList (mapMaybe unp cached), NamedResolverRef name uncached)
  where
    resolveCached ctx key = (cachedArg key, lookup key $ localCache ctx)

unp :: (a, Maybe b) -> Maybe (a, b)
unp (_, Nothing) = Nothing
unp (x, Just y) = Just (x, y)
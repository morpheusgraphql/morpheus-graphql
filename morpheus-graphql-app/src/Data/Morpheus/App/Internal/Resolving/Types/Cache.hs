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

module Data.Morpheus.App.Internal.Resolving.Types.Cache
  ( Cache,
    CacheKey (..),
    LocalCache,
    useCached,
    buildCacheWith,
    ResolverMapContext (..),
    ResolverMapT (..),
    runResMapT,
  )
where

import Control.Monad.Except (MonadError (throwError))
import Data.ByteString.Lazy.Char8 (unpack)
import qualified Data.HashMap.Lazy as HM
import Data.Morpheus.App.Internal.Resolving.Types (NamedResolverRef (..), NamedResolverResult, ResolverMap)
import Data.Morpheus.Core (RenderGQL, render)
import Data.Morpheus.Types.Internal.AST
  ( GQLError,
    SelectionContent,
    TypeName,
    VALID,
    ValidValue,
    internal,
  )
import GHC.Show (Show (show))
import Relude hiding (show)

type Cache m = HashMap CacheKey (NamedResolverResult m)

type LocalCache = HashMap CacheKey ValidValue

useCached :: (Eq k, Hashable k, MonadError GQLError f) => HashMap k a -> k -> f a
useCached mp v = case HM.lookup v mp of
  Just x -> pure x
  Nothing -> throwError (internal "TODO:")

dumpCache :: Bool -> LocalCache -> a -> a
dumpCache enabled xs a
  | null xs || not enabled = a
  | otherwise = trace ("\nCACHE:\n" <> intercalate "\n" (map printKeyValue $ HM.toList xs) <> "\n") a
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

instance Show CacheKey where
  show (CacheKey sel typename dep) = printSel sel <> ":" <> toString typename <> ":" <> unpack (render dep)

instance Hashable CacheKey where
  hashWithSalt s (CacheKey sel tyName arg) = hashWithSalt s (sel, tyName, render arg)

uniq :: (Eq a, Hashable a) => [a] -> [a]
uniq = HM.keys . HM.fromList . map (,True)

buildBatches :: [(SelectionContent VALID, NamedResolverRef)] -> [BatchEntry]
buildBatches inputs =
  let entityTypes = uniq $ map (second resolverTypeName) inputs
   in mapMaybe (selectByEntity inputs) entityTypes

selectByEntity :: [(SelectionContent VALID, NamedResolverRef)] -> (SelectionContent VALID, TypeName) -> Maybe BatchEntry
selectByEntity inputs (tSel, tName) = case filter areEq inputs of
  [] -> Nothing
  xs -> Just $ BatchEntry tSel tName (uniq $ concatMap (resolverArgument . snd) xs)
  where
    areEq (sel, v) = sel == tSel && tName == resolverTypeName v

type ResolverFun m = NamedResolverRef -> SelectionContent VALID -> m [ValidValue]

resolveBatched :: Monad m => ResolverFun m -> BatchEntry -> m LocalCache
resolveBatched f (BatchEntry sel name deps) = do
  res <- f (NamedResolverRef name deps) sel
  let keys = map (CacheKey sel name) deps
  let entries = zip keys res
  pure $ HM.fromList entries

updateCache :: (Monad m, Traversable t) => ResolverFun m -> LocalCache -> t BatchEntry -> m LocalCache
updateCache f cache entries = do
  caches <- traverse (resolveBatched f) entries
  let newCache = foldr (<>) cache caches
  pure $ dumpCache False newCache newCache

buildCacheWith :: Monad m => ResolverFun m -> LocalCache -> [(SelectionContent VALID, NamedResolverRef)] -> m LocalCache
buildCacheWith f cache entries = updateCache f cache (buildBatches entries)

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
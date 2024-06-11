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
  ( ResolverMapT (..),
    SelectionRef,
    runBatchedT,
    MonadBatching (..),
  )
where

import Control.Monad.Except (MonadError (throwError))
import Data.ByteString.Lazy.Char8 (unpack)
import Data.HashMap.Lazy (keys)
import Data.Morpheus.App.Internal.Resolving.Cache
  ( CacheKey (..),
    CacheT,
    CacheValue (..),
    cacheResolverValues,
    cacheValue,
    isCached,
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
  ( NamedResolverRef (..),
    ResolverMonad,
    ResolverValue (ResEnum, ResNull, ResObject, ResRef, ResScalar),
  )
import Data.Morpheus.Core (render)
import Data.Morpheus.Internal.Utils (Empty (empty), IsMap (..), selectOr)
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

data BatchEntry = BatchEntry
  { batchedSelection :: SelectionContent VALID,
    batchedType :: TypeName,
    batchedArguments :: [ValidValue]
  }

instance Show BatchEntry where
  show BatchEntry {..} =
    "\nBATCH("
      <> toString batchedType
      <> "):"
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

newtype ResolverMapT m a = ResolverMapT
  { _runResMapT :: ReaderT (ResolverMap m) (CacheT m) a
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
  lift = ResolverMapT . lift . lift

deriving instance (MonadError GQLError m) => MonadError GQLError (ResolverMapT m)

runBatchedT :: (Monad m) => ResolverMapT m a -> ResolverMap m -> m a
runBatchedT (ResolverMapT m) rmap = fst <$> runStateT (runReaderT m rmap) empty

toKeys :: BatchEntry -> [CacheKey]
toKeys (BatchEntry sel name deps) = map (CacheKey sel name) deps

inCache :: (Monad m) => CacheT m a -> ResolverMapT m a
inCache = ResolverMapT . lift

class (MonadTrans t) => MonadBatching t where
  resolveRef :: (ResolverMonad m) => SelectionContent VALID -> NamedResolverRef -> t m (CacheKey, CacheValue m)
  storeValue :: (ResolverMonad m) => CacheKey -> ValidValue -> t m ValidValue

instance MonadBatching IdentityT where
  resolveRef _ _ = throwError $ internal "batching is only allowed with named resolvers"
  storeValue _ _ = throwError $ internal "batching is only allowed with named resolvers"

instance MonadBatching ResolverMapT where
  resolveRef sel (NamedResolverRef typename [arg]) = do
    let key = CacheKey sel typename arg
    alreadyCached <- inCache (isCached key)
    if alreadyCached
      then pure ()
      else prefetch (BatchEntry sel typename [arg])
    inCache $ do
      value <- useCached key
      pure (key, value)
  resolveRef _ ref = throwError (internal ("expected only one resolved value for " <> msg (show ref :: String)))
  storeValue key = inCache . cacheValue key

prefetch :: (ResolverMonad m) => BatchEntry -> ResolverMapT m ()
prefetch batch = do
  value <- run batch
  batches <- buildBatches . concat <$> traverse (lift . scanRefs (batchedSelection batch)) value
  resolvedEntries <- traverse (\b -> (b,) <$> run b) batches
  let caches = foldMap zipCaches $ (batch, value) : resolvedEntries
  inCache $ cacheResolverValues caches
  where
    zipCaches (b, res) = zip (toKeys b) res
    run = withDebug >=> runBatch

runBatch :: (MonadError GQLError m, MonadReader ResolverContext m) => BatchEntry -> ResolverMapT m [ResolverValue m]
runBatch (BatchEntry _ name deps)
  | null deps = pure []
  | otherwise = do
      resolvers <- ResolverMapT ask
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

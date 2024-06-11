{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.App.Internal.Resolving.Cache
  ( CacheKey (..),
    CacheStore (..),
    printSelectionKey,
    useCached,
    isCached,
    withDebug,
    cacheResolverValues,
    cacheValue,
    CacheValue (..),
    CacheT,
  )
where

import Control.Monad.Except
import Data.ByteString.Lazy.Char8 (unpack)
import qualified Data.HashMap.Lazy as HM
import Data.Morpheus.App.Internal.Resolving.ResolverState
import Data.Morpheus.App.Internal.Resolving.Types (ResolverValue)
import Data.Morpheus.App.Internal.Resolving.Utils (ResolverMonad)
import Data.Morpheus.Core (Config (debug), RenderGQL, render)
import Data.Morpheus.Internal.Utils
  ( Empty (..),
    IsMap (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( Msg (msg),
    SelectionContent,
    TypeName,
    VALID,
    ValidValue,
    internal,
  )
import Debug.Trace (trace)
import Relude hiding (Show, empty, show, trace)
import Prelude (Show (show))

type CacheT m = (StateT (CacheStore m) m)

printSelectionKey :: (RenderGQL a) => a -> String
printSelectionKey sel = map replace $ filter ignoreSpaces $ unpack (render sel)
  where
    ignoreSpaces x = x /= ' '
    replace '\n' = ' '
    replace x = x

data CacheKey = CacheKey
  { cachedSel :: SelectionContent VALID,
    cachedTypeName :: TypeName,
    cachedArg :: ValidValue
  }
  deriving (Eq, Generic)

data CacheValue m
  = CachedValue ValidValue
  | CachedResolver (ResolverValue m)

instance Show (CacheValue m) where
  show (CachedValue v) = unpack (render v)
  show (CachedResolver v) = show v

instance Show CacheKey where
  show (CacheKey sel typename dep) = printSelectionKey sel <> ":" <> toString typename <> ":" <> unpack (render dep)

instance Hashable CacheKey where
  hashWithSalt s (CacheKey sel tyName arg) = hashWithSalt s (sel, tyName, render arg)

newtype CacheStore m = CacheStore {_unpackStore :: HashMap CacheKey (CacheValue m)}

instance Show (CacheStore m) where
  show (CacheStore cache) = "\nCACHE:\n" <> intercalate "\n" (map printKeyValue $ toAssoc cache) <> "\n"
    where
      printKeyValue (key, v) = " " <> show key <> ": " <> show v

instance Empty (CacheStore m) where
  empty = CacheStore empty

cacheResolverValues :: (ResolverMonad m) => [(CacheKey, ResolverValue m)] -> CacheT m ()
cacheResolverValues pres = do
  CacheStore oldCache <- get
  let updates = unsafeFromList (map (second CachedResolver) pres)
  cache <- labeledDebug "\nUPDATE|>" $ CacheStore $ updates <> oldCache
  modify (const cache)

useCached :: (ResolverMonad m) => CacheKey -> CacheT m (CacheValue m)
useCached v = do
  cache <- get >>= labeledDebug "\nUSE|>"
  case lookup v (_unpackStore cache) of
    Just x -> pure x
    Nothing -> throwError (internal $ "cache value could not found for key" <> msg (show v :: String))

isCached :: (Monad m) => CacheKey -> CacheT m Bool
isCached key = isJust . lookup key . _unpackStore <$> get

setValue :: (CacheKey, ValidValue) -> CacheStore m -> CacheStore m
setValue (key, value) = CacheStore . HM.insert key (CachedValue value) . _unpackStore

labeledDebug :: (Show a, MonadReader ResolverContext m) => String -> a -> m a
labeledDebug label v = showValue <$> asks (debug . config)
  where
    showValue enabled
      | enabled = trace (label <> show v) v
      | otherwise = v

withDebug :: (Show a, MonadReader ResolverContext m) => a -> m a
withDebug = labeledDebug ""

cacheValue :: (Monad m) => CacheKey -> ValidValue -> CacheT m ValidValue
cacheValue key value = modify (setValue (key, value)) $> value

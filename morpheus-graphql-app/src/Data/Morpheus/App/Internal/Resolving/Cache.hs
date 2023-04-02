{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.App.Internal.Resolving.Cache
  ( CacheKey (..),
    CacheStore (..),
    printSelectionKey,
    useCached,
    isNotCached,
    mergeCache,
    withDebug,
    insertPres,
    CacheValue (..),
  )
where

import Control.Monad.Except
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Morpheus.App.Internal.Resolving.ResolverState
import Data.Morpheus.App.Internal.Resolving.Types (ResolverValue)
import Data.Morpheus.Core (Config (debug), RenderGQL, render)
import Data.Morpheus.Internal.Utils
  ( Empty (..),
    IsMap (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( GQLError,
    Msg (msg),
    SelectionContent,
    TypeName,
    VALID,
    ValidValue,
    internal,
  )
import Debug.Trace (trace)
import Relude hiding (Show, empty, show, trace)
import Prelude (Show (show))

printSelectionKey :: RenderGQL a => a -> String
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

insertPres :: CacheStore m -> [(CacheKey, ResolverValue m)] -> CacheStore m
insertPres cache = mergeCache cache . pure . CacheStore . unsafeFromList . map (second CachedResolver)

useCached :: MonadError GQLError m => CacheStore m' -> CacheKey -> m (CacheValue m')
useCached (CacheStore mp) v = case lookup v mp of
  Just x -> pure x
  Nothing -> throwError (internal $ "cache value could not found for key" <> msg (show v :: String))

isNotCached :: CacheStore m -> CacheKey -> Bool
isNotCached (CacheStore store) key = isNothing $ lookup key store

mergeCache :: CacheStore m -> [CacheStore m] -> CacheStore m
mergeCache initial caches = CacheStore $ fold $ map _unpackStore (initial : caches)

withDebug :: (Show a, MonadReader ResolverContext m) => a -> m a
withDebug v = showValue <$> asks (debug . config)
  where
    showValue enabled
      | not enabled = v
      | otherwise = trace (show v) v

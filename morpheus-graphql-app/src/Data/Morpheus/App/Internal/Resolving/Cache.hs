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
    updateCache,
    CacheValue (..),
    setValue,
    CacheT,
    setCacheValue,
  )
where

import Control.Monad.Except
import Data.ByteString.Lazy.Char8 (unpack)
import qualified Data.HashMap.Lazy as HM
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

type CacheT m = (StateT (CacheStore m) m)

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

updateCache :: MonadReader ResolverContext m => [(CacheKey, ResolverValue m)] -> CacheT m ()
updateCache pres = do
  caches <- ins pres
  modify (`mergeCache` [caches])
  get >>= withDebug >> pure ()
  where
    ins = pure . CacheStore . unsafeFromList . map (second CachedResolver)

useCached :: MonadError GQLError m => CacheKey -> CacheT m (CacheValue m)
useCached v = do
  mp <- get
  case lookup v (_unpackStore mp) of
    Just x -> pure x
    Nothing -> throwError (internal $ "cache value could not found for key" <> msg (show v :: String))

isCached :: Monad m => CacheKey -> CacheT m Bool
isCached key = isJust . lookup key . _unpackStore <$> get

mergeCache :: CacheStore m -> [CacheStore m] -> CacheStore m
mergeCache initial caches = CacheStore $ fold $ map _unpackStore (initial : caches)

setValue :: (CacheKey, ValidValue) -> CacheStore m -> CacheStore m
setValue (key, value) = CacheStore . HM.insert key (CachedValue value) . _unpackStore

withDebug :: (Show a, MonadReader ResolverContext m) => a -> m a
withDebug v = showValue <$> asks (debug . config)
  where
    showValue enabled
      | enabled = v
      | otherwise = trace (show v) v

setCacheValue :: Monad m => CacheKey -> ValidValue -> CacheT m ValidValue
setCacheValue key value = modify (setValue (key, value)) $> value

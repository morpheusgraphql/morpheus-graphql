{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.App.Internal.Resolving.Cache
  ( CacheKey (..),
    CacheStore,
    printSelectionKey,
    initCache,
    useCached,
    isNotCached,
    mergeCache,
  )
where

import Control.Monad.Except
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Morpheus.Core (RenderGQL, render)
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
import Relude hiding (Show, empty, show)
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

instance Show CacheKey where
  show (CacheKey sel typename dep) = printSelectionKey sel <> ":" <> toString typename <> ":" <> unpack (render dep)

instance Hashable CacheKey where
  hashWithSalt s (CacheKey sel tyName arg) = hashWithSalt s (sel, tyName, render arg)

newtype CacheStore = CacheStore {_unpackStore :: HashMap CacheKey ValidValue}

instance Show CacheStore where
  show (CacheStore cache) = intercalate "\n" (map printKeyValue $ toAssoc cache) <> "\n"
    where
      printKeyValue (key, v) = " " <> show key <> ": " <> unpack (render v)

instance Empty CacheStore where
  empty = CacheStore empty

initCache :: Monad m => [(CacheKey, ValidValue)] -> m CacheStore
initCache = fmap CacheStore . pure . unsafeFromList

useCached :: MonadError GQLError m => CacheStore -> CacheKey -> m ValidValue
useCached (CacheStore mp) v = case lookup v mp of
  Just x -> pure x
  Nothing -> throwError (internal $ "cache value could not found for key" <> msg (show v :: String))

isNotCached :: CacheStore -> CacheKey -> Bool
isNotCached (CacheStore store) key = isNothing $ lookup key store

mergeCache :: CacheStore -> [CacheStore] -> CacheStore
mergeCache initial caches = CacheStore $ fold $ map _unpackStore (initial : caches)
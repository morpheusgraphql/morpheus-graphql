{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Mergeable.IsMap
  ( IsMap (..),
    selectBy,
    selectOr,
    FromList (..),
  )
where

import Control.Monad.Except (MonadError (throwError))
#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson.Key (Key)
import qualified Data.Aeson.KeyMap as A
#endif
{- ORMOLU_DISABLE -}
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map as M
{- ORMOLU_ENABLE -}
import Data.Mergeable.Internal.Merge (mergeNoDuplicates)
import Data.Mergeable.Internal.NameCollision (NameCollision)
import Relude

class IsMap k m | m -> k where
  unsafeFromList :: [(k, a)] -> m a

  singleton :: k -> a -> m a

  lookup :: k -> m a -> Maybe a

  member :: k -> m a -> Bool
  member = selectOr False (const True)

  toAssoc :: m a -> [(k, a)]

instance (Eq k, Hashable k) => IsMap k (HashMap k) where
  unsafeFromList = HM.fromList
  singleton = HM.singleton
  lookup = HM.lookup
  member = HM.member
  toAssoc = HM.toList

instance (Eq k, Ord k) => IsMap k (Map k) where
  unsafeFromList = M.fromList
  singleton = M.singleton
  lookup = M.lookup
  member = M.member
  toAssoc = M.toList

#if MIN_VERSION_aeson(2,0,0)
instance IsMap Key A.KeyMap where
  unsafeFromList = A.fromList
  singleton = A.singleton
  lookup = A.lookup
  member = A.member
  toAssoc = A.toList
#endif

selectBy :: (MonadError e m, IsMap k c, Monad m) => e -> k -> c a -> m a
selectBy err = selectOr (throwError err) pure

selectOr :: (IsMap k c) => d -> (a -> d) -> k -> c a -> d
selectOr fb f key lib = maybe fb f (lookup key lib)

class FromList m map k a where
  fromList :: (Monad m) => [(k, a)] -> m (map k a)

instance (Hashable k, Eq k, MonadError e m, NameCollision e a) => FromList m HashMap k a where
  fromList = mergeNoDuplicates HM.fromList

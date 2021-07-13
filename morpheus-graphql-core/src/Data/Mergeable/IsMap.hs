{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Mergeable.IsMap
  ( IsMap (..),
    selectBy,
    selectOr,
    selectOrBy,
  )
where

import qualified Data.HashMap.Lazy as HM
import Data.Morpheus.Ext.Failure (Failure (..))
import Relude

class IsMap k m | m -> k where
  fromMap :: HashMap k a -> m a
  toMap :: m a -> HashMap k a

  unsafeFromList :: [(k, a)] -> m a

  singleton :: k -> a -> m a

  lookup :: k -> m a -> Maybe a

  member :: k -> m a -> Bool
  member = selectOr False (const True)

instance (Eq k, Hashable k) => IsMap k (HashMap k) where
  fromMap = id
  toMap = id
  unsafeFromList = HM.fromList
  singleton = HM.singleton
  lookup = HM.lookup
  member = HM.member

selectBy :: (Failure e m, IsMap k c, Monad m) => e -> k -> c a -> m a
selectBy err = selectOr (failure err) pure

selectOr :: IsMap k c => d -> (a -> d) -> k -> c a -> d
selectOr fb f key lib = maybe fb f (lookup key lib)

selectOrBy :: IsMap k c => (t -> c a) -> d -> (a -> d) -> k -> t -> d
selectOrBy f' fb f key = selectOr fb f key . f'

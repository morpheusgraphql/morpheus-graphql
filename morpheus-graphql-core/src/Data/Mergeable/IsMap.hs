{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Mergeable.IsMap
  ( IsMap (..),
    selectBy,
    selectOr,
    FromList (..),
  )
where

import qualified Data.HashMap.Lazy as HM
import Data.Mergeable.Internal.Merge (mergeNoDuplicates)
import Data.Mergeable.Internal.NameCollision (NameCollision)
import Data.Morpheus.Ext.Failure (Failure (..))
import Data.Morpheus.Types.Internal.AST.Error (ValidationErrors)
import Relude

class IsMap k m | m -> k where
  unsafeFromList :: [(k, a)] -> m a

  singleton :: k -> a -> m a

  lookup :: k -> m a -> Maybe a

  member :: k -> m a -> Bool
  member = selectOr False (const True)

instance (Eq k, Hashable k) => IsMap k (HashMap k) where
  unsafeFromList = HM.fromList
  singleton = HM.singleton
  lookup = HM.lookup
  member = HM.member

selectBy :: (Failure e m, IsMap k c, Monad m) => e -> k -> c a -> m a
selectBy err = selectOr (failure err) pure

selectOr :: IsMap k c => d -> (a -> d) -> k -> c a -> d
selectOr fb f key lib = maybe fb f (lookup key lib)

class FromList m map k a where
  fromList :: (Monad m, Failure ValidationErrors m) => [(k, a)] -> m (map k a)

instance (Hashable k, Eq k, NameCollision a) => FromList m HashMap k a where
  fromList = mergeNoDuplicates HM.fromList

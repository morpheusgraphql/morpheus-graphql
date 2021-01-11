{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Ext.Selectable
  ( Selectable (..),
    selectBy,
  )
where

import qualified Data.HashMap.Lazy as HM
import Data.Morpheus.Ext.Failure (Failure (..))
import Data.Morpheus.Ext.KeyOf (KeyOf (..))
import Relude

class Selectable k a c | c -> a where
  selectOr :: d -> (a -> d) -> k -> c -> d

  member :: k -> c -> Bool
  member = selectOr False (const True)

instance KeyOf k a => Selectable k a [a] where
  selectOr fb f key lib = maybe fb f (find ((key ==) . keyOf) lib)

instance (Eq k, Hashable k) => Selectable k a (HashMap k a) where
  selectOr fb f key lib = maybe fb f (HM.lookup key lib)

selectBy :: (Failure e m, Selectable k a c, Monad m) => e -> k -> c -> m a
selectBy err = selectOr (failure err) pure

{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Internal.Ext
  ( Eventless,
    Failure (..),
    PushEvents (..),
    Result (..),
    ResultT (..),
    cleanEvents,
    resultOr,
    mapEvent,
    unpackEvents,
    sortErrors,
    unsafeFromList,
    (<:>),
    resolveWith,
    runResolutionT,
    toEither,
    Merge (..),
  )
where

import Data.Mergeable
import qualified Data.Mergeable.SafeHashMap as SHM
import Data.Morpheus.Ext.KeyOf
import qualified Data.Morpheus.Ext.OrdMap as OM
import Data.Morpheus.Ext.Result
import Data.Morpheus.Internal.Utils ((<:>))
import Relude

class UnsafeFromList f where
  unsafeFromList :: (Hashable k, KeyOf k a, Eq k) => [(k, a)] -> f k a

instance UnsafeFromList OM.OrdMap where
  unsafeFromList = OM.unsafeFromList

instance UnsafeFromList SHM.SafeHashMap where
  unsafeFromList = SHM.unsafeFromList

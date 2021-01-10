{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Internal.Ext
  ( Eventless,
    Failure (..),
    PushEvents (..),
    Result (..),
    ResultT (..),
    cleanEvents,
    resultOr,
    SemigroupM (..),
    mapEvent,
    unpackEvents,
    sortErrors,
    unsafeFromList,
    (<:>),
    resolveWith,
    runResolutionT,
  )
where

import Data.Morpheus.Ext.KeyOf
import Data.Morpheus.Ext.Map
import qualified Data.Morpheus.Ext.OrdMap as OM
import Data.Morpheus.Ext.Result
import qualified Data.Morpheus.Ext.SafeHashMap as SHM
import Data.Morpheus.Ext.SemigroupM
import Relude

class UnsafeFromList f where
  unsafeFromList :: (Hashable k, KeyOf k a, Eq k) => [(k, a)] -> f k a

instance UnsafeFromList OM.OrdMap where
  unsafeFromList = OM.unsafeFromList

instance UnsafeFromList SHM.SafeHashMap where
  unsafeFromList = SHM.unsafeFromList

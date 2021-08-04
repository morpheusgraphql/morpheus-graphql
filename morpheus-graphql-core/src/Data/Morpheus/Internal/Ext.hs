{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Internal.Ext
  ( Failure,
    PushEvents (..),
    Result (..),
    ResultT (..),
    cleanEvents,
    resultOr,
    mapEvent,
    sortErrors,
    unsafeFromList,
    (<:>),
    resolveWith,
    runResolutionT,
    toEither,
    Merge (..),
    failure,
    GQLResult,
  )
where

import Data.Mergeable
import Data.Morpheus.Ext.Result
import Data.Morpheus.Internal.Utils

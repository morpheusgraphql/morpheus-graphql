module Data.Mergeable
  ( Merge (..),
    mergeNoDuplicates,
    recursiveMerge,
    mergeConcat,
    Indexed (..),
    NameCollision (..),
    ResolutionT,
    fromListT,
    indexed,
    resolveWith,
    runResolutionT,
    collect,
    IsMap (..),
  )
where

import Data.Mergeable.Internal.Merge
import Data.Mergeable.Internal.NameCollision
import Data.Mergeable.Internal.Resolution
import Data.Mergeable.IsMap

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Mergeable.Internal.Merge
  ( mergeConcat,
    Merge (..),
    mergeNoDuplicates,
    recursiveMerge,
    collect,
  )
where

import qualified Data.HashMap.Lazy as HM
import Data.Mergeable.Internal.NameCollision (NameCollision (nameCollision))
import Data.Mergeable.Internal.Resolution
  ( fromListT,
    resolveWith,
    runResolutionT,
  )
import Data.Morpheus.Ext.Failure (Failure (failure))
import Data.Morpheus.Types.Internal.AST.Error
  ( ValidationErrors,
  )
import Relude hiding (empty, join)

class Merge m a where
  merge :: (Monad m) => a -> a -> m a

instance
  ( NameCollision a,
    Eq k,
    Hashable k,
    Failure ValidationErrors m
  ) =>
  Merge m (HashMap k a)
  where
  merge x y = mergeNoDuplicates HM.fromList (HM.toList x <> HM.toList y)

mergeConcat ::
  ( Failure ValidationErrors m,
    Merge m a,
    Monad m
  ) =>
  NonEmpty a ->
  m a
mergeConcat (value :| []) = pure value
mergeConcat (value :| (x : xs)) = do
  a <- merge value x
  mergeConcat (a :| xs)

-- Merge Object with of Failure as an Option
failOnDuplicates :: (Failure ValidationErrors m, NameCollision a) => NonEmpty a -> m a
failOnDuplicates (x :| xs)
  | null xs = pure x
  | otherwise = failure $ fmap nameCollision (x : xs)

mergeOnDuplicates ::
  ( Monad m,
    Eq a,
    Merge m a
  ) =>
  a ->
  a ->
  m a
mergeOnDuplicates oldValue newValue
  | oldValue == newValue = pure oldValue
  | otherwise = merge oldValue newValue

mergeNoDuplicates :: (Monad m, Eq k, Hashable k, Failure ValidationErrors m, NameCollision a) => ([(k, a)] -> b) -> [(k, a)] -> m b
mergeNoDuplicates f xs = runResolutionT (fromListT xs) f failOnDuplicates

recursiveMerge :: (Monad m, Hashable k, Eq k, Eq a, Merge m a) => ([(k, a)] -> b) -> [(k, a)] -> m b
recursiveMerge f xs = runResolutionT (fromListT xs) f (resolveWith mergeOnDuplicates)

collect :: (Monad m, Eq k, Hashable k, Semigroup v) => [(k, v)] -> m (HashMap k v)
collect xs = runResolutionT (fromListT xs) HM.fromList (resolveWith (\x y -> pure (x <> y)))

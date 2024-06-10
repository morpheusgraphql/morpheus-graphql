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
    throwErrors,
  )
where

import Control.Monad.Except (MonadError (throwError))
import qualified Data.HashMap.Lazy as HM
import Data.Mergeable.Internal.NameCollision (NameCollision (nameCollision))
import Data.Mergeable.Internal.Resolution
  ( fromListT,
    resolveWith,
    runResolutionT,
  )
import Relude hiding (empty, join)

class Merge m a where
  merge :: (Monad m) => a -> a -> m a

instance
  ( Eq k,
    Hashable k,
    MonadError e m,
    NameCollision e a
  ) =>
  Merge m (HashMap k a)
  where
  merge x y = mergeNoDuplicates HM.fromList (HM.toList x <> HM.toList y)

mergeConcat ::
  ( Monad m,
    Merge m a,
    MonadError e m
  ) =>
  NonEmpty a ->
  m a
mergeConcat (value :| []) = pure value
mergeConcat (value :| (x : xs)) = do
  a <- merge value x
  mergeConcat (a :| xs)

throwErrors :: (MonadError e m) => NonEmpty e -> m b
throwErrors (e :| es) = throwError e <* traverse throwError es

-- Merge Object with of Failure as an Option
failOnDuplicates :: (MonadError e m, NameCollision e a) => NonEmpty a -> m a
failOnDuplicates (x :| xs)
  | null xs = pure x
  | otherwise = throwErrors (nameCollision <$> x :| xs)

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

mergeNoDuplicates ::
  ( Eq k,
    Hashable k,
    Monad m,
    MonadError e m,
    NameCollision e a
  ) =>
  ([(k, a)] -> b) ->
  [(k, a)] ->
  m b
mergeNoDuplicates f xs = runResolutionT (fromListT xs) f failOnDuplicates

recursiveMerge ::
  ( Eq k,
    Eq a,
    Hashable k,
    Monad m,
    Merge m a
  ) =>
  ([(k, a)] -> b) ->
  [(k, a)] ->
  m b
recursiveMerge f xs = runResolutionT (fromListT xs) f (resolveWith mergeOnDuplicates)

collect ::
  ( Eq k,
    Hashable k,
    Monad m,
    Semigroup v
  ) =>
  [(k, v)] ->
  m (HashMap k v)
collect xs = runResolutionT (fromListT xs) HM.fromList (resolveWith (\x y -> pure (x <> y)))

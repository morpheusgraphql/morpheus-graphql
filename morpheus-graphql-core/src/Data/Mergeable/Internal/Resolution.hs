{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Mergeable.Internal.Resolution
  ( Indexed (..),
    indexed,
    fromListT,
    resolveWith,
    runResolutionT,
    ResolutionT,
  )
where

import qualified Data.HashMap.Lazy as HM
import Language.Haskell.TH.Syntax (Lift)
import Relude

sortedEntries :: [Indexed k a] -> [(k, a)]
sortedEntries = fmap f . sortOn index
  where
    f a = (indexedKey a, indexedValue a)

fromListT :: (Monad m, Eq k, Hashable k) => [(k, a)] -> ResolutionT k a coll m coll
fromListT = traverse resolveDuplicatesM . fromListDuplicates >=> fromNoDuplicatesM

resolveWith ::
  (Monad m) =>
  (a -> a -> m a) ->
  NonEmpty a ->
  m a
resolveWith f (x :| xs) = foldlM f x xs

data Indexed k a = Indexed
  { index :: Int,
    indexedKey :: k,
    indexedValue :: a
  }
  deriving
    ( Show,
      Eq,
      Functor,
      Traversable,
      Foldable,
      Lift
    )

fromListDuplicates :: (Eq k, Hashable k) => [(k, a)] -> [(k, NonEmpty a)]
fromListDuplicates xs =
  sortedEntries
    $ HM.elems
    $ clusterDuplicates (indexed xs) HM.empty

indexed :: [(k, a)] -> [Indexed k a]
indexed = __indexed 0
  where
    __indexed :: Int -> [(k, a)] -> [Indexed k a]
    __indexed _ [] = []
    __indexed i ((k, x) : xs) = Indexed i k x : __indexed (i + 1) xs

resolveDuplicatesM :: (Monad m) => (k, NonEmpty a) -> ResolutionT k a coll m (k, a)
resolveDuplicatesM (k, xs) = asks resolveDuplicates >>= lift . fmap (k,) . (xs &)

fromNoDuplicatesM :: (Monad m) => [(k, a)] -> ResolutionT k a coll m coll
fromNoDuplicatesM xs = asks ((xs &) . fromNoDuplicates)

insertWithList :: (Eq k, Hashable k) => Indexed k (NonEmpty a) -> HashMap k (Indexed k (NonEmpty a)) -> HashMap k (Indexed k (NonEmpty a))
insertWithList (Indexed i1 key value) = HM.alter (Just . updater) key
  where
    updater Nothing = Indexed i1 key value
    updater (Just (Indexed i2 _ x)) = Indexed i2 key (x <> value)

clusterDuplicates :: (Eq k, Hashable k) => [Indexed k a] -> HashMap k (Indexed k (NonEmpty a)) -> HashMap k (Indexed k (NonEmpty a))
clusterDuplicates [] = id
clusterDuplicates xs = flip (foldl' (\coll x -> insertWithList (fmap (:| []) x) coll)) xs

data Resolution k a coll m = Resolution
  { resolveDuplicates :: NonEmpty a -> m a,
    fromNoDuplicates :: [(k, a)] -> coll
  }

runResolutionT ::
  ResolutionT k a coll m b ->
  ([(k, a)] -> coll) ->
  (NonEmpty a -> m a) ->
  m b
runResolutionT (ResolutionT x) fromNoDuplicates resolveDuplicates = runReaderT x Resolution {..}

newtype ResolutionT k a coll m x = ResolutionT
  { _runResolutionT :: ReaderT (Resolution k a coll m) m x
  }
  deriving
    ( Functor,
      Monad,
      Applicative,
      MonadReader (Resolution k a coll m)
    )

instance MonadTrans (ResolutionT k a coll) where
  lift = ResolutionT . lift

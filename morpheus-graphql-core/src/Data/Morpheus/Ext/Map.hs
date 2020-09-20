{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Ext.Map
  ( Indexed (..),
    indexed,
    fromListT,
    resolveWith,
    runResolutionT,
    ResolutionT,
  )
where

import Control.Applicative (Applicative (..))
import Control.Monad ((>=>), (>>=), Monad)
import Control.Monad.Reader (MonadReader (..), asks)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Reader
  ( ReaderT (..),
  )
import Data.Foldable (Foldable, foldl, foldlM)
import Data.Function ((&))
import Data.Functor (Functor (..))
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.Hashable (Hashable)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (Maybe (..))
import Data.Semigroup (Semigroup ((<>)))
import Data.Traversable (Traversable (..))
import Language.Haskell.TH.Syntax (Lift)
import Prelude
  ( ($),
    (+),
    (.),
    Eq,
    Int,
    Show,
    flip,
    id,
    snd,
  )

sortedEntries :: [Indexed k a] -> [(k, a)]
sortedEntries = fmap f . sortOn index
  where
    f a = (indexedKey a, indexedValue a)

fromListT :: (Monad m, Eq k, Hashable k) => [(k, a)] -> ResolutionT a coll m coll
fromListT = traverse (resolveDuplicatesM . snd) . fromListDuplicates >=> fromNoDuplicatesM

resolveWith ::
  Monad m =>
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

resolveDuplicatesM :: Monad m => NonEmpty a -> ResolutionT a coll m a
resolveDuplicatesM xs = asks resolveDuplicates >>= lift . (xs &)

fromNoDuplicatesM :: Monad m => [a] -> ResolutionT a coll m coll
fromNoDuplicatesM xs = asks ((xs &) . fromNoDuplicates)

insertWithList :: (Eq k, Hashable k) => Indexed k (NonEmpty a) -> HashMap k (Indexed k (NonEmpty a)) -> HashMap k (Indexed k (NonEmpty a))
insertWithList (Indexed i1 key value) = HM.alter (Just . updater) key
  where
    updater Nothing = Indexed i1 key value
    updater (Just (Indexed i2 _ x)) = Indexed i2 key (x <> value)

clusterDuplicates :: (Eq k, Hashable k) => [Indexed k a] -> HashMap k (Indexed k (NonEmpty a)) -> HashMap k (Indexed k (NonEmpty a))
clusterDuplicates [] = id
clusterDuplicates xs = flip (foldl (\coll x -> insertWithList (fmap (:| []) x) coll)) xs

data Resolution a coll m = Resolution
  { resolveDuplicates :: NonEmpty a -> m a,
    fromNoDuplicates :: [a] -> coll
  }

runResolutionT ::
  ResolutionT a coll m b ->
  ([a] -> coll) ->
  (NonEmpty a -> m a) ->
  m b
runResolutionT (ResolutionT x) fromNoDuplicates resolveDuplicates = runReaderT x Resolution {..}

newtype ResolutionT a coll m x = ResolutionT
  { _runResolutionT :: ReaderT (Resolution a coll m) m x
  }
  deriving
    ( Functor,
      Monad,
      Applicative,
      MonadReader (Resolution a coll m)
    )

instance MonadTrans (ResolutionT e coll) where
  lift = ResolutionT . lift

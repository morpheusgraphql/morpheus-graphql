{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Mergeable.MergeMap
  ( MergeMap,
    toNonEmpty,
    partition,
  )
where

import Control.Monad.Except (MonadError (..))
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE (partition)
import qualified Data.List.NonEmpty as NM
import Data.Mergeable.Internal.Merge
  ( Merge (..),
    recursiveMerge,
  )
import Data.Mergeable.IsMap (FromList (..), IsMap (..))
import Language.Haskell.TH.Syntax (Lift (..))
import Relude hiding (fromList)

partition :: (a -> Bool) -> MergeMap dups k a -> (Maybe (MergeMap dups k a), Maybe (MergeMap dups k a))
partition f (MergeMap xs) =
  case NE.partition (f . snd) xs of
    ([], _) -> (Nothing, Just (MergeMap xs))
    (_, []) -> (Just (MergeMap xs), Nothing)
    (a : as, b : bs) -> (Just (MergeMap (a :| as)), Just (MergeMap (b :| bs)))

newtype MergeMap (dups :: Bool) k a = MergeMap
  { unpack :: NonEmpty (k, a)
  }
  deriving
    ( Show,
      Eq,
      Functor,
      Foldable,
      Traversable,
      Hashable
    )

instance (Lift a, Lift k) => Lift (MergeMap dups k a) where
  lift (MergeMap (x :| xs)) = [|MergeMap (x :| xs)|]

#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped (MergeMap (x :| xs))  = [|| MergeMap (x :| xs) ||]
#endif

instance
  (Hashable k, Eq k) =>
  IsMap k (MergeMap dups k)
  where
  unsafeFromList (x : xs) = MergeMap (x :| xs)
  unsafeFromList [] = error "empty selection sets are not supported."
  singleton k x = MergeMap ((k, x) :| [])
  lookup key (MergeMap (x :| xs)) = L.lookup key (x : xs)
  toAssoc (MergeMap (x :| xs)) = x : xs

instance
  ( Monad m,
    Eq a,
    Merge m a,
    Hashable k,
    Eq k
  ) =>
  Merge m (MergeMap 'False k a)
  where
  merge (MergeMap x) (MergeMap y) = resolveMergeable (x <> y)

instance (Monad m) => Merge m (MergeMap 'True k a) where
  merge (MergeMap x) (MergeMap y) = pure $ MergeMap $ x <> y

resolveMergeable ::
  ( Monad m,
    Eq a,
    Merge m a,
    Hashable k,
    Eq k
  ) =>
  NonEmpty (k, a) ->
  m (MergeMap dups k a)
resolveMergeable (x :| xs) = recursiveMerge (MergeMap . NM.fromList) (x : xs)

toNonEmpty :: (IsString e, MonadError e f) => [a] -> f (NonEmpty a)
toNonEmpty [] = throwError "empty selection sets are not supported."
toNonEmpty (x : xs) = pure (x :| xs)

instance
  ( Hashable k,
    Eq k,
    Eq a,
    IsString e,
    MonadError e m,
    Merge m a
  ) =>
  FromList m (MergeMap 'False) k a
  where
  fromList = resolveMergeable <=< toNonEmpty

instance
  ( IsString e,
    MonadError e m
  ) =>
  FromList m (MergeMap 'True) k a
  where
  fromList = fmap MergeMap . toNonEmpty

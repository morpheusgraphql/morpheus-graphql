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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Ext.MergeSet
  ( MergeMap,
    toNonEmpty,
  )
where

import qualified Data.List as L
import Data.Mergeable
  ( Merge (..),
    recursiveMerge,
  )
import Data.Mergeable.IsMap (IsMap (..))
import Data.Morpheus.Internal.Utils
  ( Failure (..),
    FromElems (..),
    KeyOf (..),
    toPair,
  )
import Data.Morpheus.Types.Internal.AST.Error
  ( ValidationError,
    ValidationErrors,
  )
import Language.Haskell.TH.Syntax (Lift (..))
import Relude

newtype MergeMap (dups :: Bool) k a = MergeSet
  { unpack :: NonEmpty (k, a)
  }
  deriving
    ( Show,
      Eq,
      Functor,
      Foldable,
      Traversable
    )

instance (Lift a, Lift k) => Lift (MergeMap dups k a) where
  lift (MergeSet (x :| xs)) = [|MergeSet (x :| xs)|]

#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped (MergeSet (x :| xs))  = [|| MergeSet (x :| xs) ||]
#endif

instance
  (Hashable k, Eq k) =>
  IsMap k (MergeMap dups k)
  where
  unsafeFromList (x : xs) = MergeSet (x :| xs)
  unsafeFromList [] = error "empty selection sets are not supported."
  singleton k x = MergeSet ((k, x) :| [])
  lookup key (MergeSet (x :| xs)) = L.lookup key (x : xs)

instance
  ( Monad m,
    Eq a,
    Merge m a,
    Hashable k,
    Eq k
  ) =>
  Merge m (MergeMap 'False k a)
  where
  merge (MergeSet x) (MergeSet y) = resolveMergeable (x <> y)

instance Monad m => Merge m (MergeMap 'True k a) where
  merge (MergeSet x) (MergeSet y) = pure $ MergeSet $ x <> y

resolveMergeable ::
  ( Monad m,
    Eq a,
    Merge m a,
    Hashable k,
    Eq k
  ) =>
  NonEmpty (k, a) ->
  m (MergeMap dups k a)
resolveMergeable (x :| xs) = recursiveMerge (MergeSet . fromList) (x : xs)

toNonEmpty :: Failure ValidationErrors f => [a] -> f (NonEmpty a)
toNonEmpty [] = failure ["empty selection sets are not supported." :: ValidationError]
toNonEmpty (x : xs) = pure (x :| xs)

instance
  ( KeyOf k a,
    Monad m,
    Failure ValidationErrors m,
    Merge m a,
    Eq a
  ) =>
  FromElems m a (MergeMap 'False k a)
  where
  fromElems = resolveMergeable . fmap toPair <=< toNonEmpty

instance (Applicative m, Failure ValidationErrors m, KeyOf k a) => FromElems m a (MergeMap 'True k a) where
  fromElems = fmap (MergeSet . fmap toPair) . toNonEmpty

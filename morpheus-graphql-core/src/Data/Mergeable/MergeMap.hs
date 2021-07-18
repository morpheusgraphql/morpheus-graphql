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

module Data.Mergeable.MergeMap
  ( MergeMap,
    toNonEmpty,
  )
where

import qualified Data.List as L
import qualified Data.List.NonEmpty as NM
import Data.Mergeable.Internal.Merge
  ( Merge (..),
    recursiveMerge,
  )
import Data.Mergeable.IsMap (FromList (..), IsMap (..))
import Data.Morpheus.Ext.Failure (Failure (..))
import Data.Morpheus.Types.Internal.AST.Error
  ( ValidationError,
    ValidationErrors,
  )
import Language.Haskell.TH.Syntax (Lift (..))
import Relude hiding (fromList)

newtype MergeMap (dups :: Bool) k a = MergeMap
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

instance Monad m => Merge m (MergeMap 'True k a) where
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

toNonEmpty :: Failure ValidationErrors f => [a] -> f (NonEmpty a)
toNonEmpty [] = failure ["empty selection sets are not supported." :: ValidationError]
toNonEmpty (x : xs) = pure (x :| xs)

instance (Merge m a, Hashable k, Eq k, Eq a) => FromList m (MergeMap 'False) k a where
  fromList = resolveMergeable <=< toNonEmpty

instance FromList m (MergeMap 'True) k a where
  fromList = fmap MergeMap . toNonEmpty

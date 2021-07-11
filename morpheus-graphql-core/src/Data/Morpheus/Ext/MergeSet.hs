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
  ( MergeSet,
    toNonEmpty,
  )
where

import qualified Data.List as L
import Data.Mergeable
  ( Merge (..),
    recursiveMerge,
  )
import Data.Morpheus.Internal.Utils
  ( Collection (singleton),
    Failure (..),
    FromElems (..),
    IsMap (..),
    KeyOf (..),
    toPair,
  )
import Data.Morpheus.Types.Internal.AST.Error
  ( ValidationError,
    ValidationErrors,
  )
import Data.Morpheus.Types.Internal.AST.Stage
  ( RAW,
    Stage,
    VALID,
  )
import Language.Haskell.TH.Syntax (Lift (..))
import Relude

-- set with mergeable components
newtype MergeSet (s :: Stage) k a = MergeSet
  { unpack :: NonEmpty (k, a)
  }
  deriving
    ( Show,
      Eq,
      Functor,
      Foldable,
      Traversable
    )

instance KeyOf k a => Collection a (MergeSet s k a) where
  singleton = MergeSet . (:| []) . toPair

instance (Lift a, Lift k) => Lift (MergeSet (s :: Stage) k a) where
  lift (MergeSet (x :| xs)) = [|MergeSet (x :| xs)|]

#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped (MergeSet (x :| xs))  = [|| MergeSet (x :| xs) ||]
#endif

instance
  ( Hashable k,
    Eq k
  ) =>
  IsMap k (MergeSet opt k)
  where
  lookup key (MergeSet (x :| xs)) = L.lookup key (x : xs)

instance
  ( Merge m a,
    Monad m,
    Failure ValidationErrors m,
    Eq a,
    Hashable k,
    Eq k
  ) =>
  Merge m (MergeSet VALID k a)
  where
  merge (MergeSet x) (MergeSet y) = resolveMergeable (x <> y)

resolveMergeable ::
  ( Monad m,
    Eq a,
    Merge m a,
    Hashable k,
    Eq k
  ) =>
  NonEmpty (k, a) ->
  m (MergeSet s k a)
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
  FromElems m a (MergeSet VALID k a)
  where
  fromElems = resolveMergeable . fmap toPair <=< toNonEmpty

instance Monad m => Merge m (MergeSet RAW k a) where
  merge (MergeSet x) (MergeSet y) = pure $ MergeSet $ x <> y

instance (Applicative m, Failure ValidationErrors m, KeyOf k a) => FromElems m a (MergeSet RAW k a) where
  fromElems = fmap (MergeSet . fmap toPair) . toNonEmpty

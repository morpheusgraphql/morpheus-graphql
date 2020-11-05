{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Ext.MergeSet
  ( MergeSet,
  )
where

import Data.Morpheus.Ext.Elems (Elems (..))
import Data.Morpheus.Ext.Map
  ( fromListT,
    resolveWith,
    runResolutionT,
  )
import Data.Morpheus.Ext.SemigroupM
  ( SemigroupM (..),
  )
import Data.Morpheus.Internal.Utils
  ( Collection (..),
    Failure (..),
    FromElems (..),
    KeyOf (..),
    Selectable (..),
    toPair,
  )
import Data.Morpheus.Types.Internal.AST.Base
  ( Ref,
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
newtype MergeSet (dups :: Stage) k a = MergeSet
  { unpack :: [a]
  }
  deriving
    ( Show,
      Eq,
      Functor,
      Foldable,
      Lift,
      Traversable,
      Collection a,
      Elems a
    )

instance (KeyOf k a) => Selectable k a (MergeSet opt k a) where
  selectOr fb f key (MergeSet ls) = maybe fb f (find ((key ==) . keyOf) ls)

instance
  ( KeyOf k a,
    SemigroupM m a,
    Monad m,
    Failure ValidationErrors m,
    Eq a
  ) =>
  SemigroupM m (MergeSet VALID k a)
  where
  mergeM path (MergeSet x) (MergeSet y) = resolveMergable path (x <> y)

resolveMergable ::
  ( KeyOf k a,
    Monad m,
    Eq a,
    SemigroupM m a,
    Failure ValidationErrors m
  ) =>
  [Ref] ->
  [a] ->
  m (MergeSet dups k a)
resolveMergable path xs = runResolutionT (fromListT (toPair <$> xs)) (MergeSet . fmap snd) (resolveWith (resolveConflict path))

instance
  ( KeyOf k a,
    SemigroupM m a,
    Monad m,
    Failure ValidationErrors m,
    Eq a
  ) =>
  FromElems m a (MergeSet VALID k a)
  where
  fromElems = resolveMergable []

instance Applicative m => SemigroupM m (MergeSet RAW k a) where
  mergeM _ (MergeSet x) (MergeSet y) = pure $ MergeSet $ x <> y

instance Applicative m => FromElems m a (MergeSet RAW k a) where
  fromElems = pure . MergeSet

resolveConflict :: (Monad m, Eq a, KeyOf k a, SemigroupM m a, Failure ValidationErrors m) => [Ref] -> a -> a -> m a
resolveConflict path oldValue newValue
  | oldValue == newValue = pure oldValue
  | otherwise = mergeM path oldValue newValue

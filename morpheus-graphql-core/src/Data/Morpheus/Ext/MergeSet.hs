{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Ext.MergeSet
  ( MergeSet,
  )
where

import Control.Applicative (Applicative (..))
import Control.Monad (Monad (..))
import Data.Foldable (Foldable (..))
import Data.Functor ((<$>), Functor (..))
import Data.List (find)
import Data.Maybe (maybe)
import Data.Morpheus.Ext.Map
  ( fromListT,
    resolveWith,
    runResolutionT,
  )
import Data.Morpheus.Internal.Utils
  ( Collection (..),
    Failure (..),
    KeyOf (..),
    Listable (..),
    Selectable (..),
    SemigroupM (..),
    elems,
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
import Data.Semigroup (Semigroup (..))
import Data.Traversable (Traversable (..))
import Language.Haskell.TH.Syntax (Lift (..))
import Prelude
  ( ($),
    (.),
    Eq (..),
    Show,
    otherwise,
    snd,
  )

-- set with mergeable components
newtype MergeSet (dups :: Stage) a = MergeSet
  { unpack :: [a]
  }
  deriving
    ( Show,
      Eq,
      Functor,
      Foldable,
      Lift,
      Traversable,
      Collection a
    )

instance (KeyOf k a) => Selectable k a (MergeSet opt a) where
  selectOr fb f key (MergeSet ls) = maybe fb f (find ((key ==) . keyOf) ls)

instance
  ( KeyOf k a,
    Listable a (MergeSet VALID a),
    SemigroupM m a,
    Eq a
  ) =>
  SemigroupM m (MergeSet VALID a)
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
  m (MergeSet dups a)
resolveMergable path xs = runResolutionT (fromListT (toPair <$> xs)) (MergeSet . fmap snd) (resolveWith (resolveConflict path))

instance
  ( Listable a (MergeSet VALID a),
    KeyOf k a,
    SemigroupM m a,
    Eq a
  ) =>
  Listable a (MergeSet VALID a)
  where
  fromElems = resolveMergable []
  elems = unpack

instance Applicative m => SemigroupM m (MergeSet RAW a) where
  mergeM _ (MergeSet x) (MergeSet y) = pure $ MergeSet $ x <> y

instance Listable a (MergeSet RAW a) where
  fromElems = pure . MergeSet
  elems = unpack

resolveConflict :: (Monad m, Eq a, KeyOf k a, SemigroupM m a, Failure ValidationErrors m) => [Ref] -> a -> a -> m a
resolveConflict path oldValue newValue
  | oldValue == newValue = pure oldValue
  | otherwise = mergeM path oldValue newValue

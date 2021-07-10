{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveTraversable #-}
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
  )
import Data.Morpheus.Types.Internal.AST.Error
  ( ValidationError,
    ValidationErrors,
  )
import Data.Morpheus.Types.Internal.AST.Name
  ( FieldName,
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
  { unpack :: NonEmpty a
  }
  deriving
    ( Show,
      Eq,
      Functor,
      Foldable,
      Traversable,
      Collection a,
      Elems a
    )

instance Lift a => Lift (MergeSet (dups :: Stage) k a) where
  lift (MergeSet (x :| xs)) = [|MergeSet (x :| xs)|]

#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped (MergeSet (x :| xs))  = [|| MergeSet (x :| xs) ||]
#endif

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
  [Ref FieldName] ->
  NonEmpty a ->
  m (MergeSet dups k a)
resolveMergable path (x :| xs) = runResolutionT (fromListT (toPair <$> (x : xs))) (MergeSet . fromList . map snd) (resolveWith (resolveConflict path))

toNonEmpty :: Failure ValidationErrors f => [a] -> f (NonEmpty a)
toNonEmpty [] = failure ["empty selection sets are not supported." :: ValidationError]
toNonEmpty (x : xs) = pure (x :| xs)

instance
  ( KeyOf k a,
    SemigroupM m a,
    Monad m,
    Failure ValidationErrors m,
    Eq a
  ) =>
  FromElems m a (MergeSet VALID k a)
  where
  fromElems = resolveMergable [] <=< toNonEmpty

instance Applicative m => SemigroupM m (MergeSet RAW k a) where
  mergeM _ (MergeSet x) (MergeSet y) = pure $ MergeSet $ x <> y

instance (Applicative m, Failure ValidationErrors m) => FromElems m a (MergeSet RAW k a) where
  fromElems = fmap MergeSet . toNonEmpty

resolveConflict ::
  ( Monad m,
    Eq a,
    KeyOf k a,
    SemigroupM m a,
    Failure ValidationErrors m
  ) =>
  [Ref FieldName] ->
  a ->
  a ->
  m a
resolveConflict path oldValue newValue
  | oldValue == newValue = pure oldValue
  | otherwise = mergeM path oldValue newValue

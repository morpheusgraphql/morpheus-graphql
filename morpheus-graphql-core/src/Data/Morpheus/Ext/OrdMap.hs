{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Ext.OrdMap
  ( OrdMap (..),
    unsafeFromList,
  )
where

import qualified Data.HashMap.Lazy as HM
import Data.Mergeable
  ( Indexed (..),
    Merge (..),
    NameCollision (..),
    indexed,
  )
import Data.Morpheus.Ext.Empty (Empty (..))
import Data.Morpheus.Internal.Utils
  ( Collection (..),
    Failure,
    FromElems (..),
    IsMap (..),
    KeyOf (..),
    toPair,
  )
import Data.Morpheus.Types.Internal.AST.Error (ValidationErrors)
import Language.Haskell.TH.Syntax (Lift (..))
import Relude

-- OrdMap
newtype OrdMap k a = OrdMap
  { mapEntries :: HashMap k (Indexed k a)
  }
  deriving
    ( Show,
      Eq,
      Functor,
      Traversable,
      Empty
    )

instance (Lift a, Lift k, Eq k, Hashable k) => Lift (OrdMap k a) where
  lift (OrdMap x) = [|OrdMap (HM.fromList ls)|]
    where
      ls = HM.toList x

#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped (OrdMap x) = [||OrdMap (HM.fromList ls)||]
    where
      ls = HM.toList x
#endif

instance (Eq k, Hashable k) => Foldable (OrdMap k) where
  foldMap f = foldMap f . getElements

getElements :: (Eq k, Hashable k) => OrdMap k b -> [b]
getElements = fmap indexedValue . sortOn index . toList . mapEntries

instance (KeyOf k a, Hashable k) => Collection a (OrdMap k a) where
  singleton x = OrdMap $ HM.singleton (keyOf x) (Indexed 0 (keyOf x) x)

instance (Eq k, Hashable k) => IsMap k (OrdMap k) where
  lookup key OrdMap {mapEntries} = indexedValue <$> lookup key mapEntries

instance (NameCollision a, Monad m, KeyOf k a, Failure ValidationErrors m) => Merge m (OrdMap k a) where
  merge (OrdMap x) (OrdMap y) = OrdMap <$> merge x y

instance (NameCollision a, Monad m, Failure ValidationErrors m, KeyOf k a, Hashable k) => FromElems m a (OrdMap k a) where
  fromElems values = OrdMap <$> fromElems (indexed (toPair <$> values))

unsafeFromList ::
  (Hashable k, Eq k) =>
  [(k, a)] ->
  OrdMap k a
unsafeFromList = OrdMap . HM.fromList . fmap withKey . indexed
  where
    withKey idx = (indexedKey idx, idx)

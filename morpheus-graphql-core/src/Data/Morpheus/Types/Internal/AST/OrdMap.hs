{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
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
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.AST.OrdMap
  ( OrdMap (..),
    unsafeFromValues,
  )
where

-- MORPHEUS

import Data.Foldable (Foldable (..))
import Data.Functor ((<$>), Functor (..))
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.Hashable (Hashable)
import Data.List (sortOn)
import Data.Maybe (maybe)
import Data.Morpheus.Error.NameCollision (NameCollision (..))
import Data.Morpheus.Ext.Map
  ( Indexed (..),
    indexed,
  )
import Data.Morpheus.Internal.Utils
  ( Collection (..),
    KeyOf (..),
    Listable (..),
    Merge (..),
    Selectable (..),
    toPair,
  )
import Data.Traversable (Traversable (..))
import Language.Haskell.TH.Syntax (Lift (..))
import Prelude
  ( ($),
    (.),
    Eq,
    Show,
  )

-- OrdMap
newtype OrdMap k a = OrdMap
  { mapEntries :: HashMap k (Indexed k a)
  }
  deriving
    ( Show,
      Eq,
      Functor,
      Traversable
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
  empty = OrdMap HM.empty
  singleton x = OrdMap $ HM.singleton (keyOf x) (Indexed 0 (keyOf x) x)

instance (Eq k, Hashable k) => Selectable k a (OrdMap k a) where
  selectOr fb f key OrdMap {mapEntries} = maybe fb (f . indexedValue) (HM.lookup key mapEntries)

instance (NameCollision a, KeyOf k a) => Merge (OrdMap k a) where
  merge ref (OrdMap x) (OrdMap y) = OrdMap <$> merge ref x y

instance (NameCollision a, KeyOf k a, Hashable k) => Listable a (OrdMap k a) where
  fromElems values = OrdMap <$> fromElems (indexed (toPair <$> values))
  elems = getElements

unsafeFromValues ::
  ( KeyOf k a,
    Hashable k
  ) =>
  [a] ->
  OrdMap k a
unsafeFromValues = OrdMap . HM.fromList . fmap withKey . indexed . fmap toPair
  where
    withKey idx = (indexedKey idx, idx)

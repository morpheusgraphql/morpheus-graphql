{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Mergeable.OrdMap
  ( OrdMap,
    ordMapDelete,
  )
where

import Control.Monad.Except (MonadError)
import qualified Data.HashMap.Lazy as HM
import Data.List ((\\))
import Data.Mergeable.Internal.Merge (Merge (..))
import Data.Mergeable.Internal.NameCollision (NameCollision (..))
import Data.Mergeable.IsMap
  ( FromList (..),
    IsMap (..),
  )
import Data.Morpheus.Ext.Empty (Empty (..))
import Language.Haskell.TH.Syntax (Lift (..))
import Relude hiding (fromList)

-- OrdMap
data OrdMap k a = OrdMap
  { order :: [k],
    entries :: HashMap k a
  }
  deriving
    ( Show,
      Eq,
      Functor,
      Traversable,
      Generic,
      Hashable
    )

instance Empty (OrdMap k a) where
  empty = OrdMap [] HM.empty

instance (Lift a, Lift k, Eq k, Hashable k) => Lift (OrdMap k a) where
  lift (OrdMap ks xs) = [|OrdMap ks (HM.fromList ls)|]
    where
      ls = HM.toList xs

#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped (OrdMap ks x) = [||OrdMap ks (HM.fromList ls)||]
    where
      ls = HM.toList x
#endif

instance (Eq k, Hashable k) => Foldable (OrdMap k) where
  foldMap f OrdMap {order, entries} = foldMap f (mapMaybe (`HM.lookup` entries) order)

instance (Eq k, Hashable k) => IsMap k (OrdMap k) where
  unsafeFromList xs = OrdMap (map fst xs) (unsafeFromList xs)
  singleton k x = OrdMap [k] (singleton k x)
  lookup key OrdMap {entries} = lookup key entries
  toAssoc OrdMap {order, entries} = mapMaybe (\k -> (k,) <$> HM.lookup k entries) order

instance (NameCollision e a, Eq k, Hashable k, Monad m, MonadError e m) => Merge m (OrdMap k a) where
  merge (OrdMap ks1 x) (OrdMap ks2 y) = OrdMap (mergeOrder ks1 ks2) <$> merge x y

mergeOrder :: (Eq a) => [a] -> [a] -> [a]
mergeOrder ks1 ks2 = ks1 <> (ks2 \\ ks1)

instance
  ( Hashable k,
    Eq k,
    NameCollision e a,
    MonadError e m
  ) =>
  FromList m OrdMap k a
  where
  fromList xs = OrdMap (map fst xs) <$> fromList xs

ordMapDelete :: (Eq k, Hashable k) => k -> OrdMap k a -> OrdMap k a
ordMapDelete k (OrdMap order entries) =
  OrdMap (filter (k /=) order) (HM.delete k entries)
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Utils.GScan
  ( scanFree,
    FreeCatType (..),
    freeLeaf,
    freeNode,
    ScanRef,
  )
where

import Data.Morpheus.Generic
  ( CBox (..),
    Gmap,
    ProxyMap (..),
    ScanRef (..),
    scan,
  )
import Data.Morpheus.Server.Deriving.Utils.Kinded (mapCat)
import Data.Morpheus.Server.Deriving.Utils.SchemaBuilder (TypeFingerprint)
import Data.Morpheus.Server.Deriving.Utils.Types (CatType (..))
import GHC.Generics (Generic (Rep))
import Relude hiding (fromList)

instance ProxyMap FreeCatType where
  proxyMap prx (FreeCatType cat) = FreeCatType (mapCat prx cat)

data FreeCatType a where
  FreeCatType :: forall c a. CatType c a -> FreeCatType a

freeLeaf :: (c1 a) => TypeFingerprint -> CatType c2 a -> ScanRef FreeCatType c1
freeLeaf fp p = ScanLeaf (show fp) (FreeCatType p)

freeNode :: (c a, Gmap c (Rep a)) => Bool -> TypeFingerprint -> CatType c2 a -> ScanRef FreeCatType c
freeNode visible fp p = ScanNode visible (show fp) (FreeCatType p)

scanFree :: (c a) => (forall k' a'. (c a') => CatType k' a' -> [ScanRef FreeCatType c]) -> CatType k a -> [CBox FreeCatType c]
scanFree f = scan (\(FreeCatType x) -> f x) . FreeCatType

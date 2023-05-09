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
  ( ScanRef (..),
    ScanProxy (..),
    scan,
    useProxies,
    FreeCatType (..),
    leafRef,
    nodeRef,
  )
where

import Data.HashMap.Strict (fromList, insert, member)
import Data.Morpheus.Generic (CProxy (..), Gmap, gmap)
import Data.Morpheus.Server.Deriving.Utils.Kinded (CatType, mapCat)
import Data.Morpheus.Server.Deriving.Utils.SchemaBuilder (TypeFingerprint)
import GHC.Generics (Generic (Rep))
import Relude hiding (fromList)

instance FreeMap FreeCatType where
  freeMap prx (FreeCatType cat) = FreeCatType (mapCat prx cat)

data FreeCatType a where
  FreeCatType :: forall c a. CatType c a -> FreeCatType a

leafRef :: (c1 a) => TypeFingerprint -> CatType c2 a -> ScanRef FreeCatType c1
leafRef fp p = ScanLeaf (show fp) (FreeCatType p)

nodeRef :: (c1 a, Gmap c1 (Rep a)) => Bool -> TypeFingerprint -> CatType c2 a -> ScanRef FreeCatType c1
nodeRef visible fp p = ScanNode visible (show fp) (FreeCatType p)

scan :: (c a) => (forall k' a'. (c a') => CatType k' a' -> [ScanRef FreeCatType c]) -> CatType k a -> [ScanProxy FreeCatType c]
scan f = gScan (\(FreeCatType x) -> f x) . FreeCatType

--  GENERIC
type Key = Text

useProxies :: (Hashable k, Eq k) => (ScanProxy f c -> [v]) -> (v -> k) -> [ScanProxy f c] -> HashMap k v
useProxies toValue toKey = fromList . map (\x -> (toKey x, x)) . concatMap toValue

gScan :: (c a, FreeMap f) => (forall a'. (c a') => f a' -> [ScanRef f c]) -> f a -> [ScanProxy f c]
gScan f = toList . scanRefs (Scanner f) mempty . f

type ScannerMap f c = HashMap Key (ScanProxy f c)

class FreeMap p where
  freeMap :: f b -> p a -> p b

runProxy :: (FreeMap f) => f a -> Scanner f c -> CProxy c -> [ScanRef f c]
runProxy cat scanner (CProxy prx) = runScanner scanner (freeMap prx cat)

fieldRefs :: (FreeMap f) => Scanner f c -> ScanRef f c -> [ScanRef f c]
fieldRefs scanner (ScanNode _ _ prx) = concatMap (runProxy prx scanner) (gmap prx)
fieldRefs _ ScanLeaf {} = []

visited :: HashMap Key v -> ScanRef f c -> Bool
visited lib (ScanNode _ fp _) = member fp lib
visited lib (ScanLeaf fp _) = member fp lib

getFingerprint :: ScanRef f c -> Key
getFingerprint (ScanNode _ fp _) = fp
getFingerprint (ScanLeaf fp _) = fp

scanRefs :: (FreeMap f) => Scanner f c -> ScannerMap f c -> [ScanRef f c] -> ScannerMap f c
scanRefs _ lib [] = lib
scanRefs ctx lib (x : xs) = do
  let values = runRef x
  let newLib = foldr (insert (getFingerprint x)) lib values
  let refs = filter (not . visited newLib) (xs <> fieldRefs ctx x)
  scanRefs ctx newLib refs

data ScanProxy f (c :: Type -> Constraint) where
  ScanProxy :: (c a) => f a -> ScanProxy f c

runRef :: ScanRef f c -> [ScanProxy f c]
runRef (ScanNode visible _ p) = [ScanProxy p | visible]
runRef (ScanLeaf _ p) = [ScanProxy p]

data ScanRef f (c :: Type -> Constraint) where
  ScanNode :: forall a f c. (Gmap c (Rep a), c a) => Bool -> Key -> f a -> ScanRef f c
  ScanLeaf :: forall a f c. (c a) => Key -> f a -> ScanRef f c

newtype Scanner f (c :: Type -> Constraint) = Scanner
  {runScanner :: forall a. (c a) => f a -> [ScanRef f c]}

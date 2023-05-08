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
import Data.Morpheus.Server.Types.TypeName (TypeFingerprint)
import GHC.Generics (Generic (Rep))
import Relude hiding (fromList)

type ScannerMap c = HashMap TypeFingerprint (ScanProxy c)

useProxies :: (Hashable k, Eq k) => (ScanProxy c -> [v]) -> (v -> k) -> [ScanProxy c] -> HashMap k v
useProxies toValue toKey = fromList . map (\x -> (toKey x, x)) . concatMap toValue

scan :: (c a) => (forall k' a'. (c a') => CatType k' a' -> [ScanRef c]) -> CatType k a -> [ScanProxy c]
scan f = toList . scanRefs (Scanner (\(FreeCatType x) -> f x)) mempty . f

runProxy :: FreeCatType a -> Scanner c -> CProxy c -> [ScanRef c]
runProxy cat scanner (CProxy prx) = runScanner scanner (mapFreeCat prx cat)

fieldRefs :: Scanner c -> ScanRef c -> [ScanRef c]
fieldRefs scanner (ScanNode _ _ prx) = concatMap (runProxy prx scanner) (gmap prx)
fieldRefs _ ScanLeaf {} = []

visited :: HashMap TypeFingerprint v -> ScanRef c -> Bool
visited lib (ScanNode _ fp _) = member fp lib
visited lib (ScanLeaf fp _) = member fp lib

getFingerprint :: ScanRef c -> TypeFingerprint
getFingerprint (ScanNode _ fp _) = fp
getFingerprint (ScanLeaf fp _) = fp

scanRefs :: Scanner c -> ScannerMap c -> [ScanRef c] -> ScannerMap c
scanRefs _ lib [] = lib
scanRefs ctx lib (x : xs) = do
  let values = runRef x
  let newLib = foldr (insert (getFingerprint x)) lib values
  let refs = filter (not . visited newLib) (xs <> fieldRefs ctx x)
  scanRefs ctx newLib refs

data ScanProxy (c :: Type -> Constraint) where
  ScanProxy :: (c a) => FreeCatType a -> ScanProxy c

runRef :: ScanRef c -> [ScanProxy c]
runRef (ScanNode visible _ p)
  | visible = [ScanProxy p]
  | otherwise = []
runRef (ScanLeaf _ p) = [ScanProxy p]

data FreeCatType a where
  FreeCatType :: forall c a. CatType c a -> FreeCatType a

mapFreeCat :: f b -> FreeCatType a -> FreeCatType b
mapFreeCat prx (FreeCatType cat) = FreeCatType (mapCat prx cat)

data ScanRef (c :: Type -> Constraint) where
  ScanNode :: forall a c. (Gmap c (Rep a), c a) => Bool -> TypeFingerprint -> FreeCatType a -> ScanRef c
  ScanLeaf :: forall a c. (c a) => TypeFingerprint -> FreeCatType a -> ScanRef c

leafRef :: (c1 a) => TypeFingerprint -> CatType c2 a -> ScanRef c1
leafRef fp p = ScanLeaf fp (FreeCatType p)

nodeRef :: (c1 a, Gmap c1 (Rep a)) => Bool -> TypeFingerprint -> CatType c2 a -> ScanRef c1
nodeRef visible fp p = ScanNode visible fp (FreeCatType p)

newtype Scanner (c :: Type -> Constraint) = Scanner
  { runScanner :: forall a. (c a) => FreeCatType a -> [ScanRef c]
  }

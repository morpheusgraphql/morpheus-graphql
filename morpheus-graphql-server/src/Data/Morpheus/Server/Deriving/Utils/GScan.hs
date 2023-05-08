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
  ( Scanner (..),
    ScanRef (..),
    scan,
    useProxies,
    ScanProxy (..),
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

scan :: Scanner c -> [ScanRef c] -> [ScanProxy c]
scan ctx = toList . scanRefs ctx mempty

runProxy :: CatType k a -> Scanner c -> CProxy c -> [ScanRef c]
runProxy cat scanner (CProxy prx) = runScanner scanner (mapCat prx cat)

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
  ScanProxy :: (c a) => CatType k a -> ScanProxy c

runRef :: ScanRef c -> [ScanProxy c]
runRef (ScanNode visible _ p)
  | visible = [ScanProxy p]
  | otherwise = []
runRef (ScanLeaf _ p) = [ScanProxy p]

data ScanRef (c :: Type -> Constraint) where
  ScanNode :: forall k a c. (Gmap c (Rep a), c a) => Bool -> TypeFingerprint -> CatType k a -> ScanRef c
  ScanLeaf :: forall k a c. (c a) => TypeFingerprint -> CatType k a -> ScanRef c

newtype Scanner (c :: Type -> Constraint) = Scanner
  { runScanner :: forall k a. (c a) => CatType k a -> [ScanRef c]
  }

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
import Data.Morpheus.Generic.Gmap
  ( Gmap,
    GmapFun (..),
    runGmap,
  )
import Data.Morpheus.Server.Deriving.Utils.Kinded (CatType (InputType, OutputType), inputType, outputType)
import Data.Morpheus.Server.Types.TypeName (TypeFingerprint)
import GHC.Generics (Generic (Rep))
import Relude hiding (fromList)

useProxies :: (Hashable k, Eq k) => (ScanProxy c -> [v]) -> (v -> k) -> [ScanProxy c] -> HashMap k v
useProxies toValue toKey = fromList . map (\x -> (toKey x, x)) . concatMap toValue

scan :: Scanner c -> [ScanRef c] -> [ScanProxy c]
scan ctx = toList . scanRefs ctx mempty

mapContext :: CatType k a -> Scanner c -> GmapFun c [ScanRef c]
mapContext OutputType (Scanner f) = GmapFun (f . outputType)
mapContext InputType (Scanner f) = GmapFun (f . inputType)

fieldRefs :: Scanner c -> ScanRef c -> [ScanRef c]
fieldRefs ctx (ScanNode _ _ x) = runGmap (rep x) (mapContext x ctx)
fieldRefs _ ScanLeaf {} = []

rep :: f a -> Proxy (Rep a)
rep _ = Proxy

visited :: HashMap TypeFingerprint v -> ScanRef c -> Bool
visited lib (ScanNode _ fp _) = member fp lib
visited lib (ScanLeaf fp _) = member fp lib

getFingerprint :: ScanRef c -> TypeFingerprint
getFingerprint (ScanNode _ fp _) = fp
getFingerprint (ScanLeaf fp _) = fp

type ProxyLib c = HashMap TypeFingerprint (ScanProxy c)

scanRefs :: Scanner c -> ProxyLib c -> [ScanRef c] -> ProxyLib c
scanRefs _ lib [] = lib
scanRefs ctx lib (x : xs) = do
  let values = runRef x
  let newLib = foldr (insert (getFingerprint x)) lib values
  let refs = filter (not . visited newLib) (xs <> fieldRefs ctx x)
  scanRefs ctx newLib refs

data ScanProxy (c :: Type -> Constraint) where
  ScanProxy :: c a => CatType k a -> ScanProxy c

runRef :: ScanRef c -> [ScanProxy c]
runRef (ScanNode visible _ p)
  | visible = [ScanProxy p]
  | otherwise = []
runRef (ScanLeaf _ p) = [ScanProxy p]

data ScanRef (c :: Type -> Constraint) where
  ScanNode :: forall k a c. (Gmap c (Rep a), c a) => Bool -> TypeFingerprint -> CatType k a -> ScanRef c
  ScanLeaf :: forall k a c. (c a) => TypeFingerprint -> CatType k a -> ScanRef c

newtype Scanner (c :: Type -> Constraint) = Scanner
  { scannerRefs :: forall k a. (c a) => CatType k a -> [ScanRef c]
  }

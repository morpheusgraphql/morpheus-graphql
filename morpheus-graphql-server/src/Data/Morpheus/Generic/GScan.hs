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

module Data.Morpheus.Generic.GScan
  ( ScanRef (..),
    ProxyMap (..),
    IsFingerprint (..),
    useProxies,
    scan,
    scanNode,
    scanLeaf,
  )
where

import Data.HashMap.Strict (fromList, insert, member)
import Data.Morpheus.Generic.Gmap (Gmap, gmap)
import Data.Morpheus.Generic.Proxy
  ( CBox (..),
    CProxy (..),
  )
import GHC.Generics (Generic (Rep))
import Relude hiding (fromList)

class IsFingerprint a where
  toFingerprint :: a -> Fingerprint

--  GENERIC
type Fingerprint = Text

useProxies :: (Hashable k, Eq k) => (CBox f c -> [v]) -> (v -> k) -> [CBox f c] -> HashMap k v
useProxies toValue toKey = fromList . map (\x -> (toKey x, x)) . concatMap toValue

scan :: (c a, ProxyMap f) => (forall a'. (c a') => f a' -> [ScanRef f c]) -> f a -> [CBox f c]
scan f = toList . scanRefs (Scanner f) mempty . f

type ScannerMap f c = HashMap Fingerprint (CBox f c)

class ProxyMap p where
  proxyMap :: f b -> p a -> p b

instance ProxyMap Proxy where
  proxyMap p _ = toProxy p
    where
      toProxy :: f a -> Proxy a
      toProxy _ = Proxy

runProxy :: (ProxyMap f) => f a -> Scanner f c -> CProxy c -> [ScanRef f c]
runProxy cat scanner (CProxy prx) = runScanner scanner (proxyMap prx cat)

fieldRefs :: (ProxyMap f) => Scanner f c -> ScanRef f c -> [ScanRef f c]
fieldRefs scanner (ScanNode _ _ prx) = concatMap (runProxy prx scanner) (gmap prx)
fieldRefs _ ScanLeaf {} = []

visited :: HashMap Fingerprint v -> ScanRef f c -> Bool
visited lib (ScanNode _ fp _) = member fp lib
visited lib (ScanLeaf fp _) = member fp lib

getFingerprint :: ScanRef f c -> Fingerprint
getFingerprint (ScanNode _ fp _) = fp
getFingerprint (ScanLeaf fp _) = fp

scanRefs :: (ProxyMap f) => Scanner f c -> ScannerMap f c -> [ScanRef f c] -> ScannerMap f c
scanRefs _ lib [] = lib
scanRefs ctx lib (x : xs) = do
  let values = runRef x
  let newLib = foldr (insert (getFingerprint x)) lib values
  let refs = filter (not . visited newLib) (xs <> fieldRefs ctx x)
  scanRefs ctx newLib refs

runRef :: ScanRef f c -> [CBox f c]
runRef (ScanNode visible _ p) = [CBox p | visible]
runRef (ScanLeaf _ p) = [CBox p]

scanLeaf :: (c a, Show fp) => fp -> f a -> ScanRef f c
scanLeaf fp = ScanLeaf (show fp)

scanNode :: (Gmap c (Rep a), c a, Show fp) => Bool -> fp -> f a -> ScanRef f c
scanNode visible fp = ScanNode visible (show fp)

data ScanRef f (c :: Type -> Constraint) where
  ScanNode :: forall a f c. (Gmap c (Rep a), c a) => Bool -> Fingerprint -> f a -> ScanRef f c
  ScanLeaf :: forall a f c. (c a) => Fingerprint -> f a -> ScanRef f c

newtype Scanner f (c :: Type -> Constraint) = Scanner
  {runScanner :: forall a. (c a) => f a -> [ScanRef f c]}

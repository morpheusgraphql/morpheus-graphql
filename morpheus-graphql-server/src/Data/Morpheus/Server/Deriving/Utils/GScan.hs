{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Utils.GScan
  ( Scanner (..),
    ScanRef (..),
    scan,
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import Data.Morpheus.Server.Deriving.Utils.GFunctor (GFunctor, GFunctorContext (..), useGfmap)
import Data.Morpheus.Server.Types.TypeName (TypeFingerprint)
import GHC.Generics (Generic (Rep))
import Relude

scan :: (Hashable k, Eq k) => (b -> k) -> Scanner c b -> [ScanRef c] -> HashMap k b
scan toKey ctx = HM.fromList . map (\x -> (toKey x, x)) . toList . traverseRecs ctx mempty

exploreFields :: Scanner c v -> ScanRef c -> [ScanRef c]
exploreFields ctx (ScanObject _ x) = useGfmap (rep x) (mapCTX ctx)
exploreFields _ ScanType {} = []

rep :: f a -> Proxy (Rep a)
rep _ = Proxy

visited :: Map TypeFingerprint v -> ScanRef c -> Bool
visited lib (ScanObject fp _) = M.member fp lib
visited lib (ScanType fp _) = M.member fp lib

getFingerprint :: ScanRef c -> TypeFingerprint
getFingerprint (ScanObject fp _) = fp
getFingerprint (ScanType fp _) = fp

traverseRecs :: Scanner c v -> Map TypeFingerprint v -> [ScanRef c] -> Map TypeFingerprint v
traverseRecs _ lib [] = lib
traverseRecs ctx lib (x : xs) = do
  let values = runFun ctx x
  let newLib = foldr (M.insert (getFingerprint x)) lib values
  let refs = filter (not . visited newLib) (xs <> exploreFields ctx x)
  traverseRecs ctx newLib refs

runFun :: Scanner c v -> ScanRef c -> [v]
runFun Scanner {..} (ScanObject _ t) = scannerFun t
runFun Scanner {..} (ScanType _ t) = scannerFun t

mapCTX :: Scanner c v -> GFunctorContext c [ScanRef c]
mapCTX (Scanner _ f _) = GFunctorContext f

data ScanRef (c :: Type -> Constraint) where
  ScanObject :: forall f a c. (GFunctor c (Rep a), c a) => TypeFingerprint -> f a -> ScanRef c
  ScanType :: forall f a c. (c a) => TypeFingerprint -> f a -> ScanRef c

data Scanner (c :: Type -> Constraint) (v :: Type) = Scanner
  { scannerFun :: forall f a. (c a) => f a -> [v],
    scannerRefs :: forall f a. (c a) => f a -> [ScanRef c],
    scannerFingerprint :: forall f a. c a => f a -> TypeFingerprint
  }

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
scan toKey ctx = HM.fromList . map (\x -> (toKey x, x)) . toList . scanRefs ctx mempty

fieldRefs :: Scanner c v -> ScanRef c -> [ScanRef c]
fieldRefs ctx (ScanObject _ x) = useGfmap (rep x) (mapContext ctx)
fieldRefs _ ScanType {} = []

rep :: f a -> Proxy (Rep a)
rep _ = Proxy

visited :: Map TypeFingerprint v -> ScanRef c -> Bool
visited lib (ScanObject fp _) = M.member fp lib
visited lib (ScanType fp _) = M.member fp lib

getFingerprint :: ScanRef c -> TypeFingerprint
getFingerprint (ScanObject fp _) = fp
getFingerprint (ScanType fp _) = fp

scanRefs :: Scanner c v -> Map TypeFingerprint v -> [ScanRef c] -> Map TypeFingerprint v
scanRefs _ lib [] = lib
scanRefs ctx lib (x : xs) = do
  let values = runRef ctx x
  let newLib = foldr (M.insert (getFingerprint x)) lib values
  let refs = filter (not . visited newLib) (xs <> fieldRefs ctx x)
  scanRefs ctx newLib refs

runRef :: Scanner c v -> ScanRef c -> [v]
runRef Scanner {..} (ScanObject _ t) = scannerFun t
runRef Scanner {..} (ScanType _ t) = scannerFun t

mapContext :: Scanner c v -> GFunctorContext c [ScanRef c]
mapContext (Scanner _ f) = GFunctorContext f

data ScanRef (c :: Type -> Constraint) where
  ScanObject :: forall f a c. (GFunctor c (Rep a), c a) => TypeFingerprint -> f a -> ScanRef c
  ScanType :: forall f a c. (c a) => TypeFingerprint -> f a -> ScanRef c

data Scanner (c :: Type -> Constraint) (v :: Type) = Scanner
  { scannerFun :: forall f a. (c a) => f a -> [v],
    scannerRefs :: forall f a. (c a) => f a -> [ScanRef c]
  }

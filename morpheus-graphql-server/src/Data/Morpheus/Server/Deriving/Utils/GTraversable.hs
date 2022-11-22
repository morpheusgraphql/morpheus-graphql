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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Utils.GTraversable
  ( GmapCTX (..),
    scan,
    GmapProxy (..),
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import Data.Morpheus.Server.Deriving.Utils.GFunctor (GFunctor, GFunctorContext (..), useGfmap)
import Data.Morpheus.Server.Types.TypeName (TypeFingerprint)
import GHC.Generics (Generic (Rep))
import Relude

scan :: (Hashable k, Eq k, c a, GFunctor c (Rep a)) => (b -> k) -> GmapCTX c b -> Proxy a -> HashMap k b
scan fk fv = HM.fromList . map (\x -> (fk x, x)) . toList . explore fv

explore :: forall c f a b. (c a, GFunctor c (Rep a)) => GmapCTX c b -> f a -> Map TypeFingerprint b
explore ctx proxy = traverseRecs ctx mempty [GmapObject (gmapKey ctx proxy) proxy :: GmapProxy c]

exploreFields :: GmapCTX c v -> GmapProxy c -> [GmapProxy c]
exploreFields ctx (GmapObject _ x) = useGfmap (toRep x) (mapCTX ctx)
exploreFields _ GmapType {} = []

toRep :: f a -> Proxy (Rep a)
toRep _ = Proxy

isVisited :: Map TypeFingerprint v -> GmapProxy c -> Bool
isVisited lib (GmapObject fp _) = M.member fp lib
isVisited lib (GmapType fp _) = M.member fp lib

getFingerprint :: GmapProxy c -> TypeFingerprint
getFingerprint (GmapObject fp _) = fp
getFingerprint (GmapType fp _) = fp

traverseRecs :: GmapCTX c v -> Map TypeFingerprint v -> [GmapProxy c] -> Map TypeFingerprint v
traverseRecs _ lib [] = lib
traverseRecs ctx lib (x : xs) = do
  let values = runFun ctx x
  let newLib = foldr (M.insert (getFingerprint x)) lib values
  let refs = filter (not . isVisited lib) (xs <> exploreFields ctx x)
  traverseRecs ctx newLib refs

runFun :: GmapCTX c v -> GmapProxy c -> [v]
runFun GmapCTX {..} (GmapObject _ t) = gmapFun t
runFun GmapCTX {..} (GmapType _ t) = gmapFun t

mapCTX :: GmapCTX c v -> GFunctorContext c [GmapProxy c]
mapCTX (GmapCTX _ f _) = GFunctorContext f

data GmapProxy (c :: Type -> Constraint) where
  GmapObject :: forall f a c. (GFunctor c (Rep a), c a) => TypeFingerprint -> f a -> GmapProxy c
  GmapType :: forall f a c. (c a) => TypeFingerprint -> f a -> GmapProxy c

data GmapCTX (c :: Type -> Constraint) (v :: Type) = GmapCTX
  { gmapFun :: forall f a. (c a) => f a -> [v],
    gmapRefs :: forall f a. (c a) => f a -> [GmapProxy c],
    gmapKey :: forall f a. c a => f a -> TypeFingerprint
  }

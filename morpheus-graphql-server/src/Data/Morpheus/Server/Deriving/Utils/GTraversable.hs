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
    Gmap,
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
explore ctx proxy = traverseRecs ctx mempty [GmapProxy (gmapKey ctx proxy) proxy :: GmapProxy c]

toRep :: f a -> Proxy (Rep a)
toRep _ = Proxy

omitVisited :: Map TypeFingerprint v -> [GmapProxy c] -> [GmapProxy c]
omitVisited lib = filter (\(GmapProxy fp _) -> not (M.member fp lib))

getValues :: GmapCTX c v -> GmapProxy c -> ([v], [GmapProxy c])
getValues ctx (GmapProxy _ x) = useGfmap (toRep x) (mapCTX ctx)

traverseRecs :: GmapCTX c v -> Map TypeFingerprint v -> [GmapProxy c] -> Map TypeFingerprint v
traverseRecs _ lib [] = lib
traverseRecs ctx lib (GmapProxy fingerprint p : xs)
  | M.member fingerprint lib = do
    let (_, recs) = useGfmap (toRep p) (mapCTX ctx)
    traverseRecs ctx lib (omitVisited lib (recs <> xs))
  | otherwise = do
    let (val, recs) = gmapFun ctx p
    let (vals, tasks) = bimap concat concat $ unzip $ map (getValues ctx) recs
    let newLib = foldr (M.insert fingerprint) lib (val <> vals)
    traverseRecs ctx newLib (recs <> xs <> tasks)

mapCTX :: GmapCTX c v -> GFunctorContext c ([v], [GmapProxy c])
mapCTX (GmapCTX f _) = GFunctorContext f

data GmapProxy (c :: Type -> Constraint) where
  GmapProxy :: forall f a c. (Gmap c a) => TypeFingerprint -> f a -> GmapProxy c

data GmapCTX (c :: Type -> Constraint) (v :: Type) = GmapCTX
  { gmapFun :: forall f a. (c a) => f a -> ([v], [GmapProxy c]),
    gmapKey :: forall f a. c a => f a -> TypeFingerprint
  }

-- Map
class (GFunctor c (Rep a), c a) => Gmap (c :: Type -> Constraint) a

instance (c a, GFunctor c (Rep a)) => Gmap c a

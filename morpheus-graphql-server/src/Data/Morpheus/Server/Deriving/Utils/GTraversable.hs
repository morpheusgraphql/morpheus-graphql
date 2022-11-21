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
    Scanner (..),
    Gmap,
    AND,
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import Data.Morpheus.Server.Deriving.Utils.GFunctor (GFunctor, GFunctorContext (..), useGfmap)
import Data.Morpheus.Server.Deriving.Utils.Kinded
  ( KindedProxy (..),
  )
import Data.Morpheus.Server.NamedResolvers (NamedResolverT)
import Data.Morpheus.Server.Types.GQLType
  ( GQLType (..),
    kindedProxy,
  )
import Data.Morpheus.Server.Types.Kind
  ( CUSTOM,
    DerivingKind,
    SCALAR,
    TYPE,
    WRAPPER,
  )
import Data.Morpheus.Server.Types.TypeName (TypeFingerprint)
import GHC.Generics (Generic (Rep))
import Relude hiding (Undefined)

traverseTypes :: (Scanner c a, c a) => GmapCTX c v -> Proxy a -> Map TypeFingerprint v
traverseTypes f = gmapFun (scanner f mempty) . kindedProxy

class (Gmap (Scanner c) (KIND a) a, c a) => Scanner (c :: Type -> Constraint) (a :: Type) where
  scan :: (Hashable k, Eq k, c a) => (b -> k) -> GmapCTX c [b] -> Proxy a -> HashMap k b

instance (Gmap (Scanner c) (KIND a) a, c a) => Scanner c a where
  scan fk fv = HM.fromList . map (\x -> (fk x, x)) . join . toList . traverseTypes fv

scanner ::
  GmapCTX c v ->
  Map TypeFingerprint v ->
  GmapCTX (Scanner c) (Map TypeFingerprint v)
scanner c@GmapCTX {..} lib =
  GmapCTX
    ( \proxy -> do
        let fingerprint = gmapKey proxy
        if M.member fingerprint lib
          then lib
          else do
            let newLib = M.insert fingerprint (gmapFun proxy) lib
            gmap (scanner c newLib) (kindedProxy proxy)
    )
    gmapKey

class (c1 a, c2 a) => AND c1 c2 a

instance (c1 a, c2 a) => AND c1 c2 a

mapCTX :: GmapCTX c v -> GFunctorContext c v
mapCTX (GmapCTX f _) = GFunctorContext f

data GmapCTX (c :: Type -> Constraint) (v :: Type) = GmapCTX
  { gmapFun :: forall f a. (c a) => f a -> v,
    gmapKey :: forall f a. c a => f a -> TypeFingerprint
  }

-- Map
class Gmap (c :: Type -> Constraint) (t :: DerivingKind) a where
  gmap :: (Monoid v, Semigroup v) => GmapCTX c v -> kinded t a -> v

instance (c a) => Gmap c SCALAR a where
  gmap GmapCTX {..} _ = gmapFun (KindedProxy :: KindedProxy (KIND a) a)

instance (c a, GFunctor c (Rep a)) => Gmap c TYPE a where
  gmap ctx@GmapCTX {..} _ = gmapFun (KindedProxy :: KindedProxy (KIND a) a) <> useGfmap (Proxy @(Rep a)) (mapCTX ctx)

instance Gmap c (KIND a) a => Gmap c WRAPPER (f a) where
  gmap f _ = gmap f (KindedProxy :: KindedProxy (KIND a) a)

instance Gmap c (KIND a) a => Gmap c CUSTOM (input -> a) where
  gmap f _ = gmap f (KindedProxy :: KindedProxy (KIND a) a)

instance Gmap c (KIND a) a => Gmap c CUSTOM (NamedResolverT m a) where
  gmap f _ = gmap f (KindedProxy :: KindedProxy (KIND a) a)

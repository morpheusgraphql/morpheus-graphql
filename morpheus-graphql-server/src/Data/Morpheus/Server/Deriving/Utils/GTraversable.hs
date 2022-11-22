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
    AND,
    KindedGmap,
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import Data.Morpheus.Server.Deriving.Utils.GFunctor (GFunctor, GFunctorContext (..), useGfmap)
import Data.Morpheus.Server.NamedResolvers (NamedResolverT)
import Data.Morpheus.Server.Types.GQLType
  ( KIND,
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

class f (KIND a) a => UseKind (f :: DerivingKind -> Type -> Constraint) a

instance f (KIND a) a => UseKind f a

gmap :: (UseKind (KindedGmap c) a, Monoid v) => GmapCTX c v -> f a -> v
gmap ctx = kindedGmap ctx . kindedProxy

class (KindedGmap (Scanner c) (KIND a) a, c a) => Scanner (c :: Type -> Constraint) (a :: Type) where
  scan :: (Hashable k, Eq k, c a) => (b -> k) -> GmapCTX c [b] -> Proxy a -> HashMap k b

instance (KindedGmap (Scanner c) (KIND a) a, c a) => Scanner c a where
  scan fk fv = HM.fromList . map (\x -> (fk x, x)) . join . toList . gmapFun (scanner fv mempty)

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
            gmap (scanner c newLib) proxy
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
class KindedGmap (c :: Type -> Constraint) (t :: DerivingKind) a where
  kindedGmap :: (Monoid v, Semigroup v) => GmapCTX c v -> kinded t a -> v

instance (c a) => KindedGmap c SCALAR a where
  kindedGmap GmapCTX {..} _ = gmapFun (Proxy @a)

instance (c a, GFunctor c (Rep a)) => KindedGmap c TYPE a where
  kindedGmap ctx@GmapCTX {..} _ = gmapFun (Proxy @a) <> useGfmap (Proxy @(Rep a)) (mapCTX ctx)

instance UseKind (KindedGmap c) a => KindedGmap c WRAPPER (f a) where
  kindedGmap f _ = gmap f (Proxy @a)

instance UseKind (KindedGmap c) a => KindedGmap c CUSTOM (input -> a) where
  kindedGmap f _ = gmap f (Proxy @a)

instance UseKind (KindedGmap c) a => KindedGmap c CUSTOM (NamedResolverT m a) where
  kindedGmap f _ = gmap f (Proxy @a)

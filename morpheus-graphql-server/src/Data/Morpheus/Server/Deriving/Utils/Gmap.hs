{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Utils.Gmap
  ( GmapContext (..),
    useGmap,
    Gmap (..),
  )
where

import GHC.Generics
  ( C,
    D,
    Datatype,
    K1,
    M1,
    S,
    U1,
    type (:*:),
    type (:+:),
  )
import Relude

newtype GmapContext (fun :: Type -> Constraint) (v :: Type) = GmapContext
  { gmapFun :: forall f a. (fun a) => f a -> v
  }

useGmap :: (Gmap c a, Monoid b) => f a -> GmapContext c b -> b
useGmap x = runReader (gfmap x)

class Gmap (c :: Type -> Constraint) a where
  gfmap :: (Monoid v) => proxy a -> Reader (GmapContext c v) v

instance (Datatype d, Gmap c a) => Gmap c (M1 D d a) where
  gfmap _ = gfmap (Proxy @a)

instance (Gmap con a) => Gmap con (M1 C c a) where
  gfmap _ = gfmap (Proxy @a)

instance (Gmap c a, Gmap c b) => Gmap c (a :+: b) where
  gfmap _ = liftA2 (<>) (gfmap (Proxy @a)) (gfmap (Proxy @b))

instance (Gmap c a, Gmap c b) => Gmap c (a :*: b) where
  gfmap _ = liftA2 (<>) (gfmap (Proxy @a)) (gfmap (Proxy @b))

instance (c a) => Gmap c (M1 S s (K1 x a)) where
  gfmap _ = ((Proxy @a) &) <$> asks gmapFun

instance Gmap c U1 where
  gfmap _ = pure mempty

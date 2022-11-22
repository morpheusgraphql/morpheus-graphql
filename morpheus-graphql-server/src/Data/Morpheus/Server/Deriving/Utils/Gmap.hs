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
  ( GFunctorContext (..),
    useGfmap,
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

newtype GFunctorContext (fun :: Type -> Constraint) (v :: Type) = GFunctorContext
  { gFunctorFun :: forall f a. (fun a) => f a -> v
  }

useGfmap :: (Gmap c a, Monoid b) => f a -> GFunctorContext c b -> b
useGfmap x = runReader (gfmap x)

class Gmap (c :: Type -> Constraint) a where
  gfmap :: (Monoid v) => proxy a -> Reader (GFunctorContext c v) v

instance (Datatype d, Gmap c a) => Gmap c (M1 D d a) where
  gfmap _ = gfmap (Proxy @a)

instance (Gmap con a) => Gmap con (M1 C c a) where
  gfmap _ = gfmap (Proxy @a)

instance (Gmap c a, Gmap c b) => Gmap c (a :+: b) where
  gfmap _ = liftA2 (<>) (gfmap (Proxy @a)) (gfmap (Proxy @b))

instance (Gmap c a, Gmap c b) => Gmap c (a :*: b) where
  gfmap _ = liftA2 (<>) (gfmap (Proxy @a)) (gfmap (Proxy @b))

instance (c a) => Gmap c (M1 S s (K1 x a)) where
  gfmap _ = ((Proxy @a) &) <$> asks gFunctorFun

instance Gmap c U1 where
  gfmap _ = pure mempty

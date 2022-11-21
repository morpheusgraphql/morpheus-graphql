{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Utils.GFunctor
  ( GFunctor,
    GFunctorContext (..),
    useGfmap,
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

useGfmap :: (GFunctor c a, Monoid b) => f a -> GFunctorContext c b -> b
useGfmap x = runReader (gfmap x)

class GFunctor (c :: Type -> Constraint) a where
  gfmap :: (Monoid v) => proxy a -> Reader (GFunctorContext c v) v

instance (Datatype d, GFunctor c a) => GFunctor c (M1 D d a) where
  gfmap _ = gfmap (Proxy @a)

instance (GFunctor con a) => GFunctor con (M1 C c a) where
  gfmap _ = gfmap (Proxy @a)

instance (GFunctor c a, GFunctor c b) => GFunctor c (a :+: b) where
  gfmap _ = liftA2 (<>) (gfmap (Proxy @a)) (gfmap (Proxy @b))

instance (GFunctor c a, GFunctor c b) => GFunctor c (a :*: b) where
  gfmap _ = liftA2 (<>) (gfmap (Proxy @a)) (gfmap (Proxy @b))

instance (c a) => GFunctor c (M1 S s (K1 x a)) where
  gfmap _ = ((Proxy @a) &) <$> asks gFunctorFun

instance GFunctor c U1 where
  gfmap _ = pure mempty

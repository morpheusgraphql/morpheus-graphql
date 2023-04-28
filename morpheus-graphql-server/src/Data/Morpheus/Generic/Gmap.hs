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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Generic.Gmap
  ( Gmap,
    GmapFun (..),
    runGmap,
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

newtype GmapFun (fun :: Type -> Constraint) (v :: Type) = GmapFun
  { gmapFun :: forall f a. (fun a) => f a -> v
  }

runGmap :: (Gmap c a, Monoid b) => f a -> GmapFun c b -> b
runGmap x = runReader (gfmap x)

runFun :: fun a => f a -> GmapFun fun v -> v
runFun x GmapFun {..} = gmapFun x

class Gmap (c :: Type -> Constraint) a where
  gfmap :: (Monoid v) => proxy a -> Reader (GmapFun c v) v

instance (Datatype d, Gmap c a) => Gmap c (M1 D d a) where
  gfmap _ = gfmap (Proxy @a)

instance (Gmap con a) => Gmap con (M1 C c a) where
  gfmap _ = gfmap (Proxy @a)

instance (Gmap c a, Gmap c b) => Gmap c (a :+: b) where
  gfmap _ = liftA2 (<>) (gfmap (Proxy @a)) (gfmap (Proxy @b))

instance (Gmap c a, Gmap c b) => Gmap c (a :*: b) where
  gfmap _ = liftA2 (<>) (gfmap (Proxy @a)) (gfmap (Proxy @b))

instance (c a) => Gmap c (M1 S s (K1 x a)) where
  gfmap _ = runFun (Proxy @a) <$> ask

instance Gmap c U1 where
  gfmap _ = pure mempty

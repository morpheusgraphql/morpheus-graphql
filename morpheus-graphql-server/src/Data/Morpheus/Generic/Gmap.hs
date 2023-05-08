{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Generic.Gmap
  ( Gmap,
    gmap,
  )
where

import Data.Morpheus.Generic.Proxy
  ( CProxy (..),
    rep,
  )
import GHC.Generics
  ( C,
    D,
    Datatype,
    Generic (..),
    K1,
    M1,
    S,
    U1,
    type (:*:),
    type (:+:),
  )
import Relude

-- newtype GmapFun (fun :: Type -> Constraint) (v :: Type) = GmapFun
--   { gmapFun :: forall f a. (fun a) => f a -> v
--   }

gmap :: (Gmap c (Rep a)) => f a -> [CProxy c]
gmap p = gfmap (rep p)

class Gmap (c :: Type -> Constraint) a where
  gfmap :: f a -> [CProxy c]

instance (Datatype d, Gmap c a) => Gmap c (M1 D d a) where
  gfmap _ = gfmap (Proxy @a)

instance (Gmap con a) => Gmap con (M1 C c a) where
  gfmap _ = gfmap (Proxy @a)

instance (Gmap c a, Gmap c b) => Gmap c (a :+: b) where
  gfmap _ = gfmap (Proxy @a) <> gfmap (Proxy @b)

instance (Gmap c a, Gmap c b) => Gmap c (a :*: b) where
  gfmap _ = gfmap (Proxy @a) <> gfmap (Proxy @b)

instance (c a) => Gmap c (M1 S s (K1 x a)) where
  gfmap _ = [CProxy (Proxy @a)]

instance Gmap c U1 where
  gfmap _ = []

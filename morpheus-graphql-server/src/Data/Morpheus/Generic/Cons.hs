{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Generic.Cons
  ( RefType (..),
    DescribeCons (..),
  )
where

import Data.Morpheus.Generic.Fields (CountFields)
import Data.Morpheus.Generic.Proxy (CProxy, conNameP)
import Data.Morpheus.Generic.RefType (RefType (..))
import GHC.Generics
import Relude

class DescribeCons con (f :: Type -> Type) where
  describeCons :: (IsString t) => Proxy f -> [(t, Maybe (CProxy con))]

instance (DescribeCons gql f) => DescribeCons gql (M1 D d f) where
  describeCons _ = describeCons (Proxy @f)

instance (DescribeCons con a, DescribeCons con b) => DescribeCons con (a :+: b) where
  describeCons _ = describeCons (Proxy @a) <> describeCons (Proxy @b)

instance (Constructor c, CountFields a, RefType con a) => DescribeCons con (M1 C c a) where
  describeCons _ = [(conNameP (Proxy @c), refType (Proxy @a))]

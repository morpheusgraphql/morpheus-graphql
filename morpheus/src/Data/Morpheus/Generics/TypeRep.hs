{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeOperators             #-}

module Data.Morpheus.Generics.TypeRep
  ( Selectors(..)
  , resolveTypes
  ) where

import           Data.Morpheus.Schema.Helpers (TypeLib)
import           Data.Proxy                        (Proxy (..))
import           GHC.Generics

shift :: a -> (a -> b) -> b
shift x y = y x

resolveTypes :: TypeLib -> [GQLTypeLib -> GQLTypeLib] -> TypeLib
resolveTypes = foldl shift

class Selectors rep t where
  getFields :: Proxy rep -> [(t, TypeLib -> TypeLib)]

instance Selectors f t => Selectors (M1 D x f) t where
  getFields _ = getFields (Proxy @f)

instance Selectors f t => Selectors (M1 C x f) t where
  getFields _ = getFields (Proxy @f)

instance (Selectors a t, Selectors b t) => Selectors (a :*: b) t where
  getFields _ = getFields (Proxy @a) ++ getFields (Proxy @b)

instance Selectors U1 t where
  getFields _ = []

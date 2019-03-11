{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeOperators             #-}

module Data.Morpheus.Generics.TypeRep
  ( Selectors(..)
  , resolveTypes
  ) where

import qualified Data.Data                         as D
import           Data.Morpheus.Types.Introspection (GQLTypeLib)
import           Data.Proxy                        (Proxy (..))
import           GHC.Generics

shift :: a -> (a -> b) -> b
shift x y = y x

resolveTypes :: GQLTypeLib -> [GQLTypeLib -> GQLTypeLib] -> GQLTypeLib
resolveTypes = foldl shift

class Selectors rep t where
  getFields :: Proxy rep -> [(t, GQLTypeLib -> GQLTypeLib)]

instance Selectors f t => Selectors (M1 D x f) t where
  getFields _ = getFields (Proxy :: Proxy f)

instance Selectors f t => Selectors (M1 C x f) t where
  getFields _ = getFields (Proxy :: Proxy f)

instance (Selectors a t, Selectors b t) => Selectors (a :*: b) t where
  getFields _ = getFields (Proxy :: Proxy a) ++ getFields (Proxy :: Proxy b)

instance Selectors U1 t where
  getFields _ = []

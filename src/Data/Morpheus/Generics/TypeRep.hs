{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables , MultiParamTypeClasses , FlexibleContexts , RankNTypes , ExistentialQuantification  #-}

module Data.Morpheus.Generics.TypeRep
    (Selectors(..), resolveTypes )
where

import              Data.Proxy                     (Proxy(..))
import              GHC.Generics
import              Data.Morpheus.Types.Introspection  (GQLTypeLib)
import  qualified   Data.Data                   as D

resolveTypes :: GQLTypeLib -> [GQLTypeLib -> GQLTypeLib] -> GQLTypeLib
resolveTypes lib []       = lib
resolveTypes lib (f : fs) = resolveTypes (f lib) fs


class  Selectors rep t where
    getFields ::  Proxy rep ->  [( t, GQLTypeLib -> GQLTypeLib )]

instance Selectors f t => Selectors (M1 D x f)  t where
    getFields _ = getFields (Proxy :: Proxy f)

instance Selectors f t => Selectors (M1 C x f) t where
    getFields _ = getFields (Proxy :: Proxy f)

instance (Selectors a t, Selectors b t ) => Selectors (a :*: b) t where
    getFields _ = getFields (Proxy :: Proxy a) ++ getFields(Proxy:: Proxy b)

instance Selectors U1 t where
    getFields _ = []


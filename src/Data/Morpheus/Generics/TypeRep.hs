{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables , MultiParamTypeClasses , FlexibleContexts , RankNTypes , ExistentialQuantification  #-}

module Data.Morpheus.Generics.TypeRep
    (Selectors(..))
where

import              Data.Proxy                     (Proxy(..))
import              GHC.Generics
import  qualified   Data.Morpheus.Types.Introspection as I
import  qualified   Data.Data                   as D

class  Selectors rep t where
    getFields ::  Proxy rep ->  [( t, I.GQLTypeLib -> I.GQLTypeLib )]

instance Selectors f t => Selectors (M1 D x f)  t where
    getFields _ = getFields (Proxy :: Proxy f)

instance Selectors f t => Selectors (M1 C x f) t where
    getFields _ = getFields (Proxy :: Proxy f)

instance (Selectors a t, Selectors b t ) => Selectors (a :*: b) t where
    getFields _ = getFields (Proxy :: Proxy a) ++ getFields(Proxy:: Proxy b)

instance Selectors U1 t where
    getFields _ = []


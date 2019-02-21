{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables , MultiParamTypeClasses , FlexibleContexts , RankNTypes , ExistentialQuantification  #-}

module Data.Morpheus.Generics.TypeRep
    (Selectors(..))
where

import           Data.ByteString                ( ByteString )
import           Data.Proxy                     ( Proxy(..) )
import           GHC.Generics
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.Morpheus.Types.Introspection
                                                ( GQL__Type(..)
                                                , GQL__Field(..)
                                                , GQL__TypeKind(..)
                                                , GQL__InputValue
                                                , GQLTypeLib
                                                )
import           Data.Data                      ( Typeable
                                                , Data
                                                , typeOf
                                                )

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


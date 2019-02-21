{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables , MultiParamTypeClasses , FlexibleContexts , RankNTypes , ExistentialQuantification  #-}

module Data.Morpheus.Generics.TypeRep
    ( Selectors(..)
    , ArgsMeta(..)
    )
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

class  Selectors rep where
    getFields ::  Proxy rep ->  [( GQL__Field, GQLTypeLib -> GQLTypeLib )]

instance Selectors f => Selectors (M1 D x f)  where
    getFields _ = getFields (Proxy :: Proxy f)

instance Selectors f => Selectors (M1 C x f)  where
    getFields _ = getFields (Proxy :: Proxy f)

instance (Selectors a, Selectors b ) => Selectors (a :*: b)  where
    getFields _ = getFields (Proxy :: Proxy a) ++ getFields(Proxy:: Proxy b)

instance Selectors U1 where
    getFields _ = []

class  ArgsMeta rep where
    getMeta ::  Proxy rep ->  [( GQL__InputValue, GQLTypeLib -> GQLTypeLib )]

instance ArgsMeta f => ArgsMeta (M1 D x f)  where
    getMeta _ = getMeta (Proxy :: Proxy f)

instance ArgsMeta f => ArgsMeta (M1 C x f)  where
    getMeta _ = getMeta (Proxy :: Proxy f)

instance (ArgsMeta a, ArgsMeta b ) => ArgsMeta (a :*: b)  where
    getMeta _ = getMeta (Proxy :: Proxy a) ++ getMeta (Proxy:: Proxy b)

instance ArgsMeta U1 where
    getMeta _ = []


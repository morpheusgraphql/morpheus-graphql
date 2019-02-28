{-# LANGUAGE  MultiParamTypeClasses ,TypeOperators , FlexibleInstances #-}

module Data.Morpheus.Generics.GDecode (GDecode(..)) where

import                 GHC.Generics
import  qualified      Data.Text              as       T
import Data.Morpheus.Types.Types (Arguments, Validation)
import  Data.Morpheus.Types.MetaInfo

fixProxy :: (a -> f a) -> f a
fixProxy f = f undefined

class GDecode i f where
    gDecode :: MetaInfo -> i -> Validation (f a)

instance GDecode i U1  where
    gDecode _ _ = pure U1

instance (Selector c, GDecode i f ) => GDecode i (M1 S c f) where
    gDecode meta gql = fixProxy (\x -> M1 <$> gDecode (meta{ key = T.pack $ selName x}) gql)

instance (Datatype c, GDecode i f)  => GDecode i (M1 D c f)  where
    gDecode meta gql  = fixProxy(\x -> M1 <$> gDecode (meta { className = T.pack $ datatypeName x}) gql)

instance GDecode i f  => GDecode i (M1 C c f)  where
    gDecode meta gql  = M1 <$> gDecode meta gql

instance (GDecode i f , GDecode i g ) => GDecode i (f :*: g)  where
    gDecode meta gql = (:*:) <$> gDecode meta gql <*> gDecode meta gql
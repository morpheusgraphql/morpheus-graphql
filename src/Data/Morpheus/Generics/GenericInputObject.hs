{-# LANGUAGE TypeOperators , FlexibleInstances #-}

module Data.Morpheus.Generics.GenericInputObject (GFromInput(..)) where

import                 GHC.Generics
import  qualified      Data.Text              as       T
import Data.Morpheus.Types.Types (Validation)
import Data.Morpheus.Types.JSType (JSType(..))
import  Data.Morpheus.Types.MetaInfo

fixProxy :: (a -> f a) -> f a
fixProxy f = f undefined

class GFromInput f where
    gFromInput :: MetaInfo -> [(T.Text,JSType)] -> Validation (f a)

instance GFromInput U1  where
    gFromInput _ _ = pure U1

instance (Selector c, GFromInput f ) => GFromInput (M1 S c f) where
    gFromInput meta gql = fixProxy (\x -> M1 <$> gFromInput (meta{ key = T.pack $ selName x}) gql)

instance (Datatype c, GFromInput f)  => GFromInput (M1 D c f)  where
    gFromInput meta gql  = fixProxy(\x -> M1 <$> gFromInput (meta { className = T.pack $ datatypeName x}) gql)

instance GFromInput f  => GFromInput (M1 C c f)  where
    gFromInput meta gql  = M1 <$> gFromInput meta gql

instance (GFromInput f , GFromInput g ) => GFromInput (f :*: g)  where
    gFromInput meta gql = (:*:) <$> gFromInput meta gql <*> gFromInput meta gql
{-# LANGUAGE TypeOperators , FlexibleInstances #-}

module Data.Morpheus.Generics.GenericInputType (GToInput(..)) where

import           GHC.Generics
import           Data.Morpheus.Types.Types     ( Arguments
                                                , Eval(..)
                                                , (::->)(Some, None)
                                                , MetaInfo(..)
                                                , Argument(..)
                                                , JSType(..)
                                                )
import           Data.Text                     ( Text, pack )

fixProxy :: (a -> f a) -> f a
fixProxy f = f undefined

class GToInput f where
    gToInput :: MetaInfo -> JSType -> Eval (f a)

instance GToInput U1  where
    gToInput _ _ = pure U1

instance (Selector c, GToInput f ) => GToInput (M1 S c f) where
    gToInput meta gql = fixProxy $ \x -> M1 <$> gToInput (meta{ key = pack $ selName x}) gql

instance (Datatype c, GToInput f)  => GToInput (M1 D c f)  where
    gToInput meta gql  = fixProxy $ \x -> M1 <$> gToInput (meta {className = pack $ datatypeName x}) gql

instance GToInput f  => GToInput (M1 C c f)  where
    gToInput meta gql  = M1 <$> gToInput meta gql

instance (GToInput f , GToInput g ) => GToInput (f :*: g)  where
    gToInput meta gql = (:*:) <$> gToInput meta gql <*> gToInput meta gql
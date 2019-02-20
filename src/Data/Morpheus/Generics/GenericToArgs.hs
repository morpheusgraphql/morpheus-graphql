{-# LANGUAGE TypeOperators , FlexibleInstances #-}

module Data.Morpheus.Generics.GenericToArgs (GToArgs(..)) where

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

class GToArgs f where
    gToArgs :: MetaInfo -> Arguments -> Eval (f a)

instance GToArgs U1  where
    gToArgs _ _ = pure U1

instance (Selector c, GToArgs f ) => GToArgs (M1 S c f) where
    gToArgs meta gql = fixProxy (\x -> M1 <$> gToArgs (meta{ key = pack $ selName x}) gql)

instance (Datatype c, GToArgs f)  => GToArgs (M1 D c f)  where
    gToArgs meta gql  = fixProxy(\x -> M1 <$> gToArgs (meta {className = pack $ datatypeName x}) gql)

instance GToArgs f  => GToArgs (M1 C c f)  where
    gToArgs meta gql  = M1 <$> gToArgs meta gql

instance (GToArgs f , GToArgs g ) => GToArgs (f :*: g)  where
    gToArgs meta gql = (:*:) <$> gToArgs meta gql <*> gToArgs meta gql
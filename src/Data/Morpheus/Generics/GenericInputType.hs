{-# LANGUAGE ScopedTypeVariables , TypeOperators , FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Generics.GenericInputType (GQLInputObject(..),GToEnum(..)) where

import           GHC.Generics
import           Data.Morpheus.Types.Types     ( Arguments
                                                , Eval(..)
                                                , (::->)(Some, None)
                                                , MetaInfo(..)
                                                , Argument(..)
                                                , JSType(..)
                                                )
import           Data.Text                     ( Text, pack )
import           Data.Proxy                     ( Proxy(..) )

fixProxy :: (a -> f a) -> f a
fixProxy f = f undefined

class GQLInputObject f where
    gToInput :: MetaInfo -> JSType -> Eval (f a)

instance GQLInputObject U1  where
    gToInput _ _ = pure U1

instance (Selector c, GQLInputObject f ) => GQLInputObject (M1 S c f) where
    gToInput meta gql = fixProxy $ \x -> M1 <$> gToInput (meta{ key = pack $ selName x}) gql

instance (Datatype c, GQLInputObject f)  => GQLInputObject (M1 D c f)  where
    gToInput meta gql  = fixProxy $ \x -> M1 <$> gToInput (meta { className = pack $ datatypeName x }) gql

instance GQLInputObject f  => GQLInputObject (M1 C c f)  where
    gToInput meta gql  = M1 <$> gToInput meta gql

instance (GQLInputObject f , GQLInputObject g ) => GQLInputObject (f :*: g)  where
    gToInput meta gql = (:*:) <$> gToInput meta gql <*> gToInput meta gql

-- TODO: handle Enum
instance (GQLInputObject a, GQLInputObject b) => GQLInputObject (a :+: b) where
    gToInput meta (JSEnum name) = L1 <$> gToInput meta (JSEnum name)
    -- L1 <$> (gToInput meta name)
   -- gToInput meta gql = R1 <$> (gToInput setting x)

-- con_Some = mkConstr ty_Resolver "Some" [] Prefix
--   con_None = mkConstr ty_Resolver "None" [] Prefix
--   ty_Resolver = mkDataType "Module.Resolver" [con_None, con_Some]

class GToEnum f where
    gToEnum :: Text -> f a
    tagName :: Proxy f -> Text

instance GToEnum U1  where
    gToEnum _  = U1
    tagName _ = ""

instance (Selector c, GToEnum f ) => GToEnum (M1 S c f) where
    gToEnum = M1 . gToEnum
    tagName _ = ""

instance (Datatype c, GToEnum f)  => GToEnum (M1 D c f)  where
    gToEnum = M1 . gToEnum
    tagName _ = ""

instance (Constructor c  , GToEnum f) => GToEnum (M1 C c f)  where
    gToEnum = M1 . gToEnum
    tagName x =  pack $ conName (undefined::(M1 C c f x))

instance (GToEnum a, GToEnum b ) => GToEnum (a :+: b) where
    gToEnum name = if  tagName (Proxy:: Proxy a) == name then L1 $ gToEnum name else R1 $ gToEnum name
    tagName _ = ""
{-# LANGUAGE ScopedTypeVariables , TypeOperators , FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Generics.GenericEnum (GToEnum(..)) where

import               GHC.Generics
import  qualified    Data.Text                    as T
import               Data.Proxy                     ( Proxy(..) )

class GToEnum f where
    gToEnum :: T.Text -> f a
    tagName :: Proxy f -> T.Text
    getTags :: proxy f -> [T.Text]

instance (Datatype c, GToEnum f)  => GToEnum (M1 D c f)  where
    gToEnum = M1 . gToEnum
    tagName _ = ""
    getTags _ = getTags (Proxy:: Proxy f)

instance (Constructor c ) => GToEnum (M1 C c U1)  where
    gToEnum _ = M1 U1
    tagName x =  T.pack $ conName (undefined::(M1 C c U1 x))
    getTags _ = [T.pack $ conName (undefined::(M1 C c U1 x))]

instance (GToEnum a, GToEnum b ) => GToEnum ( a :+: b) where
    gToEnum name = if  tagName (Proxy:: Proxy a) == name then L1 $ gToEnum name else R1 $ gToEnum name
    tagName _ = ""
    getTags _ = getTags (Proxy:: Proxy a) ++ getTags (Proxy:: Proxy b)
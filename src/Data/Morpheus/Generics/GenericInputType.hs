{-# LANGUAGE ScopedTypeVariables , TypeOperators , FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Generics.GenericInputType (GToEnum(..)) where

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
import           Data.Morpheus.Types.Introspection ( GQL__Type, createType)

class GToEnum f where
    gToEnum :: Text -> f a
    tagName :: Proxy f -> Text

instance (Datatype c, GToEnum f)  => GToEnum (M1 D c f)  where
    gToEnum = M1 . gToEnum
    tagName _ = ""

instance (Constructor c ) => GToEnum (M1 C c U1)  where
    gToEnum _ = M1 U1
    tagName x =  pack $ conName (undefined::(M1 C c U1 x))

instance (GToEnum a, GToEnum b ) => GToEnum ( a :+: b) where
    gToEnum name = if  tagName (Proxy:: Proxy a) == name then L1 $ gToEnum name else R1 $ gToEnum name
    tagName _ = ""
-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes , DefaultSignatures, FlexibleContexts #-}

module Data.Morpheus.Generics.InputType
    ( GQLInput(..)
    )
where

import           Data.Morpheus.Types.Types      ( JSType(..)
                                                , MetaInfo(..)
                                                )
import           Data.Text                      ( Text
                                                , unpack
                                                )
import           Data.Morpheus.Generics.GenericInputType
                                                ( GQLInputObject(..)
                                                , GToEnum(..)
                                                )
import           GHC.Generics
import           Data.Data

class GQLInput a where
    decode :: JSType -> a
    default decode :: ( Show a  , Generic a, Data a , GToEnum (Rep a) ) => JSType -> a
    decode (JSEnum text) = to $ gToEnum text
   -- TODO:: write input Object Recognition
   -- decode (JSObject hashMap) to $ gToInput hashMap

instance GQLInput Text where
    decode  (JSString x) = x

instance GQLInput Bool where
    decode  (JSBool x) = x

instance GQLInput Int where
    decode  (JSInt x) = x

instance GQLInput a => GQLInput (Maybe a) where
    decode JSNull = Nothing
    decode x = Just (decode x)

--instance  GQLArgs Text where
--    fromArgs _ (Just t) = pure t
--    fromArgs _ _ = pure "Nothing found"
--    argsMeta _ = []

--instance  GQLArgs Int where
--    fromArgs _ (Just t) = pure t
--    fromArgs _ _ = pure 0
--    argsMeta _ = [GQL__InputValue { name = "Int", description = "", _type = Nothing, defaultValue = "" }]

--instance  GQLArgs Bool where
--    fromArgs _ (Just t) = pure t
--    fromArgs _ _ = pure False
--    argsMeta _ = [GQL__InputValue { name = "Boolean", description = "", _type = Nothing, defaultValue = "" }]

--instance  GQLArgs a => GQLArgs (Maybe a) where
--    fromArgs _ (Just t) = pure t
--    argsMeta _ = argsMeta (Proxy :: Proxy a)



{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE  ScopedTypeVariables , AllowAmbiguousTypes , DefaultSignatures, FlexibleContexts #-}

module Data.Morpheus.Generics.GQLInput
    ( GQLInput(..)
    )
where

import           Data.Morpheus.Types.Types      ( JSType(..)
                                                , MetaInfo(..)
                                                , GQLEnum(..)
                                                )
import           Data.Text                      ( Text
                                                , unpack
                                                , pack
                                                )
import           Data.Morpheus.Generics.GenericEnum ( GToEnum(..))
import           GHC.Generics
import           Data.Data
import           Data.Morpheus.Types.Introspection ( createScalar , GQLTypeLib, GQL__InputValue(..), createInputValue)
import           Data.Map as M
import           Data.Morpheus.Generics.GQLEnumType (GQLEnumType(..))

getType :: Typeable a => a -> Text
getType = pack . show . typeOf


class GQLInput a where
    decode :: JSType -> a
    -- default decode :: ( Show a  , Generic a, Data a , GToEnum (Rep a) ) => JSType -> a
    -- TODO:: write input Object Recognition
    -- decode (JSObject hashMap) to $ gToInput hashMap

    typeInfo :: Proxy a -> Text -> GQL__InputValue
    default typeInfo :: (Show a, Typeable a) => Proxy a -> Text -> GQL__InputValue
    typeInfo _ name  = createInputValue name $ getType (undefined::a)

    introInput :: Proxy a -> GQLTypeLib -> GQLTypeLib
    default introInput :: (Show a, Typeable a) => Proxy a -> GQLTypeLib -> GQLTypeLib
    introInput _  typeLib = do
            let typeName = getType (undefined::a)
            case M.lookup typeName typeLib of
                Just _ -> typeLib
                Nothing -> insert typeName (createScalar typeName) typeLib

instance GQLInput Text where
    decode  (JSString x) = x
    typeInfo _ name = createInputValue name "String"
    introInput _  typeLib = typeLib

instance GQLInput Bool where
    decode  (JSBool x) = x
    typeInfo _ name = createInputValue name "Boolean"
    introInput _  typeLib = typeLib

instance GQLInput Int where
    decode  (JSInt x) = x
    typeInfo _ name = createInputValue name "Int"
    introInput _  typeLib = typeLib

instance (GQLInput a , Show a, Typeable a ) => GQLInput (Maybe a) where
    decode JSNull = Nothing
    decode x = Just (decode x)
    typeInfo _ name =  (typeInfo (Proxy :: Proxy a) name) { defaultValue = "Nothing" }
    introInput _  typeLib = typeLib


instance ( Show a, GQLEnumType a ) => GQLInput (GQLEnum a) where
    decode (JSEnum text) = GQLEnum (decodeEnum (JSEnum text))
    typeInfo _  = enumType (Proxy :: Proxy a)
    introInput _ = introspectEnum (Proxy :: Proxy a)





{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE  ScopedTypeVariables , AllowAmbiguousTypes , DefaultSignatures, FlexibleContexts #-}

module Data.Morpheus.Generics.GQLInput
    ( GQLInput(..)
    )
where

import           Data.Morpheus.Types.Types      ( EnumOf(..) ,Validation(..))
import Data.Morpheus.Types.JSType         (JSType(..))
import Data.Morpheus.Types.MetaInfo         (MetaInfo(..))
import           Data.Text                      ( Text
                                                , unpack
                                                , pack
                                                )
import           Data.Morpheus.Generics.GenericEnum ( GToEnum(..))
import           GHC.Generics
import           Data.Data
import           Data.Morpheus.Types.Introspection ( createScalar , GQLTypeLib, GQL__InputValue(..), createInputValue)
import qualified Data.Map as M
import           Data.Morpheus.Generics.GQLEnum (GQLEnum(..))
import qualified Data.Morpheus.Schema.GQL__InputValue as I (GQL__InputValue(..))
import           Data.Morpheus.Generics.GenericInputObject (GFromInput(..))
import Data.Morpheus.Types.MetaInfo (MetaInfo(..), initialMeta)
import qualified  Data.Morpheus.ErrorMessage    as Err

getType :: Typeable a => a -> Text
getType = pack . show . typeOf

instance GQLInput a => GFromInput  (K1 i a)  where
    gFromInput meta inputObj = case lookup (key meta) inputObj of
            Nothing -> Left $ Err.requiredArgument meta
            Just x -> K1 <$> decode x

class GQLInput a where
    -- TODO:: write input Object Recognition
    decode :: JSType -> Validation a
    default decode :: ( Show a  , Generic a, Data a , GFromInput (Rep a) ) => JSType -> Validation a
    decode (JSObject hashMap) = to <$> gFromInput initialMeta (M.toList hashMap )

    typeInfo :: Proxy a -> Text -> GQL__InputValue
    default typeInfo :: (Show a, Typeable a) => Proxy a -> Text -> GQL__InputValue
    typeInfo _ name  = createInputValue name $ getType (undefined::a)

    introInput :: Proxy a -> GQLTypeLib -> GQLTypeLib
    default introInput :: (Show a, Typeable a) => Proxy a -> GQLTypeLib -> GQLTypeLib
    introInput _  typeLib = do
            let typeName = getType (undefined::a)
            case M.lookup typeName typeLib of
                Just _ -> typeLib
                Nothing -> M.insert typeName (createScalar typeName) typeLib

instance GQLInput Text where
    decode  (JSString x) = pure x
    typeInfo _ name = createInputValue name "String"
    introInput _  typeLib = typeLib

instance GQLInput Bool where
    decode  (JSBool x) = pure x
    typeInfo _ name = createInputValue name "Boolean"
    introInput _  typeLib = typeLib

instance GQLInput Int where
    decode  (JSInt x) = pure x
    typeInfo _ name = createInputValue name "Int"
    introInput _  typeLib = typeLib

instance (GQLInput a , Show a, Typeable a ) => GQLInput (Maybe a) where
    decode JSNull = pure Nothing
    decode x = Just <$> decode x
    typeInfo _ name =  (typeInfo (Proxy :: Proxy a) name) { I.defaultValue = "Nothing" }
    introInput _  typeLib = typeLib

instance ( Show a, GQLEnum a ) => GQLInput (EnumOf a) where
    decode (JSEnum text) = pure $ EnumOf (decodeEnum (JSEnum text))
    typeInfo _  = enumType (Proxy :: Proxy a)
    introInput _ = introspectEnum (Proxy :: Proxy a)





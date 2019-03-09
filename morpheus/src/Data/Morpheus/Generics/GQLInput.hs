{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE  MultiParamTypeClasses ,ScopedTypeVariables ,FlexibleInstances , AllowAmbiguousTypes , DefaultSignatures, FlexibleContexts #-}

module Data.Morpheus.Generics.GQLInput
    ( GQLInput(..)
    )
where

import           Data.Morpheus.Types.Types      ( EnumOf(..)
                                                , Validation(..)
                                                )
import           Data.Morpheus.Types.JSType     ( JSType(..) )
import           Data.Text                      ( Text
                                                , unpack
                                                , pack
                                                )
import           GHC.Generics
import           Data.Data
import           Data.Morpheus.Types.Introspection
                                                ( GQL__Field
                                                , createInputObject
                                                , createScalar
                                                , GQLTypeLib
                                                , GQL__InputValue(..)
                                                , createInputValue
                                                )
import qualified Data.Map                      as M
import           Data.Morpheus.Generics.GQLEnum ( GQLEnum(..) )
import qualified Data.Morpheus.Schema.GQL__InputValue
                                               as I
                                                ( GQL__InputValue(..) )
import           Data.Morpheus.Generics.GDecode ( GDecode(..) )
import           Data.Morpheus.Types.MetaInfo   ( MetaInfo(..)
                                                , initialMeta
                                                )
import qualified Data.Morpheus.ErrorMessage    as Err
import           Data.Morpheus.Generics.TypeRep ( Selectors(..)
                                                , resolveTypes
                                                )

getType :: Typeable a => a -> Text
getType = pack . show . typeOf

instance GQLInput a => GDecode JSType (K1 i a)  where
    gDecode meta (JSObject x) = case lookup (key meta) x of
            Nothing -> Left $ Err.requiredArgument [] meta
            Just x -> K1 <$> decode x



class GQLInput a where
    decode :: JSType -> Validation a
    default decode :: ( Show a  , Generic a, Data a , GDecode JSType (Rep a) ) => JSType -> Validation a
    decode (JSObject x) = to <$> gDecode initialMeta (JSObject x)

    typeInfo :: Proxy a -> Text -> GQL__InputValue
    default typeInfo :: (Show a, Typeable a) => Proxy a -> Text -> GQL__InputValue
    typeInfo _ name  = createInputValue name $ getType (undefined::a)

    introInput :: Proxy a -> GQLTypeLib -> GQLTypeLib
    default introInput :: (Show a, Typeable a, Selectors (Rep a) GQL__Field  ) => Proxy a -> GQLTypeLib -> GQLTypeLib
    introInput _  typeLib = case M.lookup typeName typeLib of
                Just _ -> typeLib
                Nothing -> addType
       where
          addType = resolveTypes (M.insert typeName (createInputObject typeName gqlFields) typeLib) stack
          typeName = getType (undefined::a)
          fieldTypes  = getFields (Proxy :: Proxy (Rep a))
          stack = map snd fieldTypes
          gqlFields = map fst fieldTypes

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





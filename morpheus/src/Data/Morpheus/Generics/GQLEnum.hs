{-# LANGUAGE OverloadedStrings, ScopedTypeVariables , AllowAmbiguousTypes , DefaultSignatures, FlexibleContexts #-}

module Data.Morpheus.Generics.GQLEnum (GQLEnum(decode,introspect,enumType,fieldType)) where

import           GHC.Generics
import qualified Data.Data                     as D
import qualified Data.Map                      as M
import qualified Data.Text                     as T
import           Data.Proxy                     ( Proxy(..) )
import qualified Data.Morpheus.Types.Introspection
                                               as I
import qualified Data.Morpheus.Schema.GQL__Field
                                               as F
                                                ( createFieldWith )
import           Data.Morpheus.Generics.GDecodeEnum
                                                ( GDecodeEnum(..) )
import           Data.Morpheus.Types.JSType     ( JSType(..) )
import           Data.Morpheus.Schema.GQL__DirectiveLocation
                                                ( GQL__DirectiveLocation(..) )

class GQLEnum a where
    decode :: JSType -> a
    default decode :: ( Show a  , Generic a, D.Data a , GDecodeEnum (Rep a) ) => JSType -> a
    decode (JSEnum text) = to $ gToEnum text

    typeID :: Proxy a -> T.Text
    default typeID :: D.Typeable a => Proxy a -> T.Text
    typeID _ = T.pack $ show $ D.typeOf (undefined::a)

    enumType :: Proxy a -> T.Text -> I.GQL__InputValue
    default enumType :: (Show a, D.Typeable a) => Proxy a -> T.Text -> I.GQL__InputValue
    enumType proxy name  = I.createInputValue name $ typeID proxy

    fieldType :: Proxy a -> T.Text -> I.GQL__Field
    default fieldType :: (Show a, D.Typeable a) => Proxy a -> T.Text -> I.GQL__Field
    fieldType proxy name  = F.createFieldWith  name (I.createEnum  (typeID proxy) []) []

    introspect :: Proxy a -> I.GQLTypeLib -> I.GQLTypeLib
    default introspect :: (Show a, D.Typeable a , GDecodeEnum (Rep a) ) => Proxy a -> I.GQLTypeLib -> I.GQLTypeLib
    introspect proxy  typeLib = case M.lookup typeName typeLib of
                Just _ -> typeLib
                Nothing -> M.insert typeName (I.createEnum typeName tags) typeLib
        where
            typeName = typeID proxy
            tags = getTags (Proxy:: Proxy (Rep a))

instance GQLEnum I.GQL__TypeKind where
    typeID _ = "__TypeKind"

instance GQLEnum GQL__DirectiveLocation where
    typeID _ = "__DirectiveLocation"
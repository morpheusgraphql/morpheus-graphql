{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE  ScopedTypeVariables , AllowAmbiguousTypes , DefaultSignatures, FlexibleContexts #-}

module Data.Morpheus.Generics.GQLEnumType where

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

getType :: Typeable a => a -> Text
getType = pack . show . typeOf


class GQLEnumType a where
    decodeEnum :: JSType -> a
    default decodeEnum :: ( Show a  , Generic a, Data a , GToEnum (Rep a) ) => JSType -> a
    decodeEnum (JSEnum text) = to $ gToEnum text

    enumType :: Proxy a -> Text -> GQL__InputValue
    default enumType :: (Show a, Typeable a) => Proxy a -> Text -> GQL__InputValue
    enumType _ name  = createInputValue name $ getType (undefined::a)

    introspectEnum :: Proxy a -> GQLTypeLib -> GQLTypeLib
    default introspectEnum :: (Show a, Typeable a) => Proxy a -> GQLTypeLib -> GQLTypeLib
    introspectEnum _  typeLib = do
            let typeName = getType (undefined::a)
            case M.lookup typeName typeLib of
                Just _ -> typeLib
                Nothing -> insert typeName (createScalar typeName) typeLib
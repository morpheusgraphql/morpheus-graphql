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
import           Data.Morpheus.Types.Introspection ( createEnum , GQLTypeLib, GQL__InputValue(..), createInputValue)
import           Data.Map as M
import           Data.Proxy                     ( Proxy(..) )

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
    default introspectEnum :: (Show a, Typeable a , GToEnum (Rep a) ) => Proxy a -> GQLTypeLib -> GQLTypeLib
    introspectEnum _  typeLib = do
            let typeName = getType (undefined::a)
            let tags = getTags (Proxy:: Proxy (Rep a))
            case M.lookup typeName typeLib of
                Just _ -> typeLib
                Nothing -> insert typeName (createEnum typeName tags) typeLib
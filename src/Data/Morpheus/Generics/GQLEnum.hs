{-# LANGUAGE  ScopedTypeVariables , AllowAmbiguousTypes , DefaultSignatures, FlexibleContexts #-}

module Data.Morpheus.Generics.GQLEnum where

import           GHC.Generics
import qualified Data.Data as D
import qualified Data.Map as M
import qualified Data.Text as T
import           Data.Proxy                     ( Proxy(..) )
import qualified Data.Morpheus.Types.Introspection as I
import           Data.Morpheus.Generics.GenericEnum ( GToEnum(..))
import           Data.Morpheus.Types.Types      ( JSType(..)
                                                , MetaInfo(..)
                                                )

getType :: D.Typeable a => a -> T.Text
getType = T.pack . show . D.typeOf

class GQLEnum a where
    decodeEnum :: JSType -> a
    default decodeEnum :: ( Show a  , Generic a, D.Data a , GToEnum (Rep a) ) => JSType -> a
    decodeEnum (JSEnum text) = to $ gToEnum text

    enumType :: Proxy a -> T.Text -> I.GQL__InputValue
    default enumType :: (Show a, D.Typeable a) => Proxy a -> T.Text -> I.GQL__InputValue
    enumType _ name  = I.createInputValue name $ getType (undefined::a)

    enumFieldType :: Proxy a -> T.Text -> I.GQL__Field
    default enumFieldType :: (Show a, D.Typeable a) => Proxy a -> T.Text -> I.GQL__Field
    enumFieldType _ name  = I.createFieldWith name (I.createEnum  (getType (undefined::a)) []) []

    introspectEnum :: Proxy a -> I.GQLTypeLib -> I.GQLTypeLib
    default introspectEnum :: (Show a, D.Typeable a , GToEnum (Rep a) ) => Proxy a -> I.GQLTypeLib -> I.GQLTypeLib
    introspectEnum _  typeLib = do
            let typeName = getType (undefined::a)
            let tags = getTags (Proxy:: Proxy (Rep a))
            case M.lookup typeName typeLib of
                Just _ -> typeLib
                Nothing -> M.insert typeName (I.createEnum typeName tags) typeLib
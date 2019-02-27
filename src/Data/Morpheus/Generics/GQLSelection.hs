{-# LANGUAGE DefaultSignatures , OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables , MultiParamTypeClasses, RankNTypes , DisambiguateRecordFields , FlexibleInstances , FlexibleContexts , TypeOperators #-}

module Data.Morpheus.Generics.GQLSelection
    ( GQLSelection(..)
    , wrapAsObject
    , arrayMap
    )
where

import            GHC.Generics
import            Control.Monad
import            Control.Monad.Trans.Except
import qualified  Control.Monad.Trans       as     Trans
import qualified  Data.Data                     as D
import qualified  Data.Text                     as T
import qualified  Data.Map                      as M
import            Data.Proxy
import            Data.Maybe                     ( fromMaybe )
import            Data.Morpheus.Schema.SchemaField (wrapAsListType)
import            Data.Morpheus.Types.Types     ( SelectionSet
                                                , QuerySelection(..)
                                                , (::->)(..)
                                                , ResolveIO(..)
                                                , failResolveIO
                                                , EnumOf(..)
                                                , Validation
                                                )
import Data.Morpheus.Types.JSType (JSType(..))
import qualified Data.Morpheus.ErrorMessage  as Err
import           Data.Morpheus.Generics.GQLArgs
                                                ( GQLArgs(..) )
import           Data.Morpheus.Schema.GQL__Schema
                                                ( GQL__Schema )
import           Data.Morpheus.Schema.GQL__Directive
                                                ( GQL__Directive )
import           Data.Morpheus.Schema.GQL__DirectiveLocation
                                                ( GQL__DirectiveLocation(..) )
import           Data.Morpheus.Types.Introspection
                                                ( GQL__Type(..)
                                                , GQL__Field
                                                , GQL__TypeKind(..)
                                                , GQL__InputValue
                                                , GQLTypeLib
                                                , GQL__Deprecation__Args
                                                , GQL__EnumValue
                                                , createType
                                                , createField
                                                , emptyLib
                                                , createScalar
                                                )
import           Data.Morpheus.Generics.TypeRep ( Selectors(..) )
import           Data.Morpheus.Generics.GenericMap ( GenericMap(..) )
import           Data.Morpheus.Types.MetaInfo (MetaInfo(..), initialMeta)
import           Data.Morpheus.Generics.GQLEnum (GQLEnum(..))
import qualified Data.Morpheus.Schema.GQL__Field as F (GQL__Field(..), createFieldWith)


renameSystemNames = T.replace "GQL__" "__";

getField :: MetaInfo -> SelectionSet -> Validation QuerySelection
getField meta gql = pure $ fromMaybe QNull (lookup (key meta) gql)

instance GQLSelection a => GenericMap (K1 i a)  where
    encodeFields meta gql (K1 src) = case getField meta gql of
        (Right field) -> case lookup (key meta) gql of
                Nothing -> []
                Just x -> [(key meta, encode field src)]
        _ -> []

instance (Selector s, D.Typeable a , GQLSelection a) => Selectors (M1 S s (K1 R a)) GQL__Field where
    getFields _ = [(fieldType (Proxy:: Proxy  a) name ,introspect (Proxy:: Proxy  a))]
        where name = T.pack $ selName (undefined :: M1 S s (K1 R a) ())

arrayMap :: GQLTypeLib -> [GQLTypeLib -> GQLTypeLib] -> GQLTypeLib
arrayMap lib []       = lib
arrayMap lib (f : fs) = arrayMap (f lib) fs

unwrapMonadTuple :: Monad m => (T.Text, m a) -> m (T.Text, a)
unwrapMonadTuple (text, ioa) = ioa >>= \x -> pure (text, x)

wrapAsObject :: [(T.Text, ResolveIO JSType)] -> ResolveIO JSType
wrapAsObject x = JSObject . M.fromList <$> mapM unwrapMonadTuple x

class GQLSelection a where

    encode :: QuerySelection ->  a -> ResolveIO JSType
    default encode :: ( Generic a, D.Data a, GenericMap (Rep a) , Show a) => QuerySelection -> a -> ResolveIO JSType
    encode (SelectionSet args gql) = wrapAsObject . encodeFields initialMeta gql . from
    encode (Field args key) = \x -> failResolveIO $ Err.subfieldsNotSelected $ MetaInfo "" "" key

    fieldType :: Proxy a -> T.Text -> GQL__Field
    default fieldType :: (Show a, Selectors (Rep a) GQL__Field , D.Typeable a) => Proxy a -> T.Text -> GQL__Field
    fieldType _ name  = createField name typeName []
        where typeName = renameSystemNames $ (T.pack . show . D.typeOf) (undefined::a)

    introspect :: Proxy a -> GQLTypeLib -> GQLTypeLib
    default introspect :: (Show a, Selectors (Rep a) GQL__Field , D.Typeable a) => Proxy a -> GQLTypeLib -> GQLTypeLib
    introspect _  typeLib = do
        let typeName = renameSystemNames $ (T.pack . show . D.typeOf) (undefined::a)
        case M.lookup typeName typeLib of
            Just _ -> typeLib
            Nothing -> arrayMap (M.insert typeName (createType typeName gqlFields) typeLib) stack
                where
                    fieldTypes  = getFields (Proxy :: Proxy (Rep a))
                    stack = map snd fieldTypes
                    gqlFields = map fst fieldTypes

getType :: (GQLSelection a, GQLArgs p) => (p ::-> a) -> (p ::-> a)
getType _ = TypeHolder Nothing

resolve
    :: (Show a, Show p, GQLSelection a, GQLArgs p)
    => QuerySelection
    -> p ::-> a
    -> p ::-> a
    -> ResolveIO JSType
resolve (SelectionSet gqlArgs body) (TypeHolder args) (Resolver resolver) =
    (ExceptT $ pure $ decodeArgs gqlArgs args) >>= resolver >>= encode
        (SelectionSet gqlArgs body)
resolve (Field gqlArgs field) (TypeHolder args) (Resolver resolver) =
    (ExceptT $ pure $ decodeArgs gqlArgs args) >>= resolver >>= encode
        (Field gqlArgs field)
resolve query _ (Some value) = encode query value
resolve _ _ None = ExceptT $ pure $ Err.handleError "resolver not implemented"

instance (Show a, Show p, GQLSelection a , GQLArgs p ) => GQLSelection (p ::-> a) where
    encode (SelectionSet args body) field = resolve (SelectionSet args body) (getType field) field
    encode (Field args body) field = resolve (Field args body) (getType field) field
    encode x (Resolver f) = resolve x (getType (Resolver f)) (Resolver f)
    encode x (Some a) = encode x a
    encode x None = pure JSNull
    introspect  _  typeLib = arrayMap typeLib $ args ++ fields
      where
        args = map snd $ introspectArgs (Proxy::Proxy p)
        fields = [introspect (Proxy:: Proxy  a)]
    fieldType _ name = (fieldType (Proxy:: Proxy  a) name ){ F.args = map fst $ introspectArgs (Proxy :: Proxy p) }

instance (Show a, GQLSelection a) => GQLSelection (Maybe a) where
    encode _ Nothing = pure JSNull
    encode query (Just value) = encode query value
    introspect  _ = introspect (Proxy:: Proxy  a)
    fieldType _ = fieldType (Proxy:: Proxy  a)

instance GQLSelection Int where
    encode _ =  pure . JSInt
    introspect _ = M.insert "Int" $ createScalar "Int"
    fieldType _ name =  F.createFieldWith name (createScalar "Int")  []

instance GQLSelection T.Text where
    encode _ =  pure . JSString
    introspect _ = M.insert "String" $ createScalar "String"
    fieldType _  name =  F.createFieldWith  name (createScalar "String") []

instance GQLSelection Bool where
    encode _ =  pure . JSBool
    introspect _ = M.insert "Boolean" $ createScalar "Boolean"
    fieldType _ name = F.createFieldWith name (createScalar "Boolean") []

instance GQLSelection a => GQLSelection [a] where
    encode (Field _ _) x =  pure $ JSList []
    encode query list = JSList <$> mapM (encode query) list
    introspect _ = introspect (Proxy :: Proxy  a)
    fieldType _ = wrapAsListType <$> fieldType (Proxy :: Proxy  a)

instance ( Show a, GQLEnum a ) => GQLSelection (EnumOf a) where
    encode _ = pure . JSString . T.pack . show . unpackEnum
    fieldType _  = enumFieldType (Proxy :: Proxy a)
    introspect _ = introspectEnum (Proxy :: Proxy a)

instance GQLSelection GQL__EnumValue
instance GQLSelection GQL__Type
instance GQLSelection GQL__Field
instance GQLSelection GQL__InputValue
instance GQLSelection GQL__Schema
instance GQLSelection GQL__Directive
instance GQLArgs GQL__Deprecation__Args
instance GQLEnum GQL__TypeKind
instance GQLEnum GQL__DirectiveLocation
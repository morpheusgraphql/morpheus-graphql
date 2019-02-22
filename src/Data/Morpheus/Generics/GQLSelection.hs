{-# LANGUAGE DefaultSignatures , OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables , MultiParamTypeClasses, RankNTypes , DisambiguateRecordFields , FlexibleInstances , FlexibleContexts , TypeOperators #-}

module Data.Morpheus.Generics.GQLSelection
    ( GQLSelection(..)
    , wrapAsObject
    , arrayMap
    )
where

import           Control.Monad
import           Data.List                      ( find )
import           Data.Data                      ( Data
                                                , Typeable
                                                , typeOf
                                                , TypeRep
                                                )
import           Data.Text                      ( Text(..)
                                                , pack
                                                , replace
                                                )
import           Data.Map                       ( singleton
                                                , fromList
                                                , insert
                                                , union
                                                )
import qualified Data.Map                      as M
import           GHC.Generics
import           Data.Morpheus.Types.Types     ( SelectionSet
                                                , QuerySelection(..)
                                                , (::->)(..)
                                                , Eval(..)
                                                , EvalIO(..)
                                                , MetaInfo(..)
                                                , JSType(..)
                                                , failEvalIO
                                                )
import           Data.Morpheus.ErrorMessage    ( handleError
                                                , subfieldsNotSelected
                                                )
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
                                                , GQL__Field(args)
                                                , GQL__TypeKind(..)
                                                , GQL__InputValue
                                                , GQLTypeLib
                                                , GQL__Deprication__Args
                                                , GQL__EnumValue
                                                , createType
                                                , createField
                                                , emptyLib
                                                )
import           Data.Morpheus.Generics.TypeRep
                                                ( Selectors(..) )
import           Data.Proxy
import           Data.Morpheus.Generics.GenericMap
                                                ( GenericMap(..)
                                                , getField
                                                , initMeta
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Control.Monad.Trans.Except
import           Control.Monad.Trans            ( liftIO
                                                , lift
                                                , MonadTrans
                                                )
import           Data.Morpheus.Schema.SchemaField (wrapAsListType)


renameSystemNames = replace "GQL__" "__";

instance GQLSelection a => GenericMap  (K1 i a)  where
    encodeFields meta gql (K1 src) = case getField meta gql of
        (Right field) -> case lookup (key meta) gql of
                Nothing -> []
                Just x -> [(key meta, encode field src)]
        _ -> []

instance (Selector s, Typeable a , GQLSelection a) => Selectors (M1 S s (K1 R a)) GQL__Field where
    getFields _ = [(fieldType (Proxy:: Proxy  a) name ,introspect (Proxy:: Proxy  a))]
        where name = pack $ selName (undefined :: M1 S s (K1 R a) ())

arrayMap :: GQLTypeLib -> [GQLTypeLib -> GQLTypeLib] -> GQLTypeLib
arrayMap lib []       = lib
arrayMap lib (f : fs) = arrayMap (f lib) fs

unwrapMonadTuple :: Monad m => (Text, m a) -> m (Text, a)
unwrapMonadTuple (text, ioa) = ioa >>= \x -> pure (text, x)

wrapAsObject :: [(Text, EvalIO JSType)] -> EvalIO JSType
wrapAsObject x = JSObject . fromList <$> mapM unwrapMonadTuple x

class GQLSelection a where

    encode :: QuerySelection ->  a -> EvalIO JSType
    default encode :: ( Generic a, Data a, GenericMap (Rep a) , Show a) => QuerySelection -> a -> EvalIO JSType
    encode (SelectionSet args gql) = wrapAsObject . encodeFields initMeta gql . from
    encode (Field args key) = \x -> failEvalIO $ subfieldsNotSelected x key

    fieldType :: Proxy a -> Text -> GQL__Field
    default fieldType :: (Show a, Selectors (Rep a) GQL__Field , Typeable a) => Proxy a -> Text -> GQL__Field
    fieldType _ name  = createField name typeName []
        where typeName = renameSystemNames $ (pack . show . typeOf) (undefined::a)

    introspect :: Proxy a -> GQLTypeLib -> GQLTypeLib
    default introspect :: (Show a, Selectors (Rep a) GQL__Field , Typeable a) => Proxy a -> GQLTypeLib -> GQLTypeLib
    introspect _  typeLib = do
        let typeName = renameSystemNames $ (pack . show . typeOf) (undefined::a)
        case M.lookup typeName typeLib of
            Just _ -> typeLib
            Nothing -> arrayMap (insert typeName (createType typeName gqlFields) typeLib) stack
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
    -> EvalIO JSType
resolve (SelectionSet gqlArgs body) (TypeHolder args) (Resolver resolver) =
    (ExceptT $ pure $ fromArgs gqlArgs args) >>= resolver >>= encode
        (SelectionSet gqlArgs body)
resolve (Field gqlArgs field) (TypeHolder args) (Resolver resolver) =
    (ExceptT $ pure $ fromArgs gqlArgs args) >>= resolver >>= encode
        (Field gqlArgs field)
resolve query _ (Some value) = encode query value
resolve _ _ None = ExceptT $ pure $ handleError "resolver not implemented"

instance (Show a, Show p, GQLSelection a , GQLArgs p ) => GQLSelection (p ::-> a) where
    encode (SelectionSet args body) field = resolve (SelectionSet args body) (getType field) field
    encode (Field args body) field = resolve (Field args body) (getType field) field
    encode x (Resolver f) = resolve x (getType (Resolver f)) (Resolver f)
    encode x (Some a) = encode x a
    encode x None = pure JSNull
    introspect  _  = introspect (Proxy:: Proxy  a)
    fieldType _ name = (fieldType (Proxy:: Proxy  a) name ){ args = argsMeta (Proxy :: Proxy p) }

instance (Show a, GQLSelection a) => GQLSelection (Maybe a) where
    encode _ Nothing = pure JSNull
    encode query (Just value) = encode query value
    introspect  _ = introspect (Proxy:: Proxy  a)
    fieldType _ = fieldType (Proxy:: Proxy  a)

instance GQLSelection Int where
    encode _ =  pure . JSInt
    introspect _ = insert "Int" (createType "Int" [])
    fieldType _ name =  createField name "Int" []

instance GQLSelection Text where
    encode _ =  pure . JSString
    introspect _ = insert "String" (createType "String" [])
    fieldType _  name =  createField  name "String" []

instance GQLSelection Bool where
    encode _ =  pure . JSBool
    introspect _ = insert "Boolean" (createType "Boolean" [])
    fieldType _ name = createField  name "Boolean" []

instance GQLSelection a => GQLSelection [a] where
    encode (Field _ _) x =  pure $ JSList []
    encode query list = JSList <$> mapM (encode query) list
    introspect _ = introspect (Proxy :: Proxy  a)
    fieldType _ = wrapAsListType <$> fieldType (Proxy :: Proxy  a)

instance GQLSelection GQL__EnumValue;

instance GQLSelection GQL__Type;

instance GQLSelection GQL__Field;

instance GQLSelection GQL__InputValue;

instance GQLSelection GQL__Schema;

instance GQLSelection GQL__Directive;

instance GQLSelection GQL__TypeKind where
    introspect _ = insert "__TypeKind" (createType "__TypeKind" [])
    fieldType _ name = createField name "__TypeKind" []
    encode _ = pure . JSString . pack . show

instance GQLSelection GQL__DirectiveLocation where
    introspect _  = insert "__DirectiveLocation" (createType "__DirectiveLocation" [])
    fieldType _ name = createField name "__DirectiveLocation" []
    encode _ = pure  . JSString . pack . show

instance  GQLArgs GQL__Deprication__Args;

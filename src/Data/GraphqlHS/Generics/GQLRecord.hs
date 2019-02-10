{-# LANGUAGE DefaultSignatures , OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables , MultiParamTypeClasses, RankNTypes , DisambiguateRecordFields , FlexibleInstances , FlexibleContexts , TypeOperators #-}

module Data.GraphqlHS.Generics.GQLRecord
    ( GQLRecord(..)
    , wrapAsObject
    )
where

import           Prelude                 hiding ( lookup )
import           Control.Monad
import           Data.List                      ( find )
import           Data.Data                      ( Data
                                                , Typeable
                                                , typeOf
                                                , TypeRep
                                                )
import           Data.Text                      ( Text(..)
                                                , pack
                                                )
import           Data.Map                       ( singleton
                                                , fromList
                                                , insert
                                                , lookup
                                                , union
                                                )
import           GHC.Generics
import           Data.GraphqlHS.Types.Types     ( Object
                                                , GQLValue(..)
                                                , (::->)(..)
                                                , Eval(..)
                                                , EvalIO(..)
                                                , MetaInfo(..)
                                                , GQLType(..)
                                                , GQLPrimitive(..)
                                                , Head(..)
                                                )
import           Data.GraphqlHS.ErrorMessage    ( handleError
                                                , subfieldsNotSelected
                                                )
import           Data.GraphqlHS.Generics.GQLArgs
                                                ( GQLArgs(..) )
import           Data.GraphqlHS.Schema.GQL__Schema
                                                ( GQL__Schema )
import           Data.GraphqlHS.Schema.GQL__Directive
                                                ( GQL__Directive )
import           Data.GraphqlHS.Schema.GQL__DirectiveLocation
                                                ( GQL__DirectiveLocation(..) )
import           Data.GraphqlHS.Types.Introspection
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
import           Data.GraphqlHS.Generics.TypeRep
                                                ( Selectors(..) )
import           Data.Proxy
import           Data.GraphqlHS.Generics.GenericMap
                                                ( GenericMap(..)
                                                , getField
                                                , initMeta
                                                )
import           Data.Maybe                     ( fromMaybe )

instance GQLRecord a => GenericMap  (K1 i a)  where
    transform meta gql (K1 src) = case (getField meta gql) of
        (Val field) -> case lookup (key meta) gql of
                Nothing -> []
                Just x -> [(key meta, trans field src)]
        _ -> []

instance (Selector s, Typeable a , GQLRecord a) => Selectors (M1 S s (K1 R a)) where
    getFields _ = [(fieldType (Proxy:: Proxy  a) name ,introspect (Proxy:: Proxy  a))]
        where name = pack $ selName (undefined :: M1 S s (K1 R a) ())

arrayMap :: GQLTypeLib -> [GQLTypeLib -> GQLTypeLib] -> GQLTypeLib
arrayMap lib []       = lib
arrayMap lib (f : fs) = arrayMap (f lib) fs

unwrapMonadTuple :: Monad m => (Text, m a) -> m (Text, a)
unwrapMonadTuple (text, ioa) = ioa >>= \x -> pure (text, x)

wrapAsObject :: [(Text, EvalIO GQLType)] -> EvalIO GQLType
wrapAsObject x = (Obj . fromList) <$> mapM unwrapMonadTuple x

class GQLRecord a where

    trans :: GQLValue ->  a -> EvalIO GQLType
    default trans :: ( Generic a, Data a, GenericMap (Rep a) , Show a) => GQLValue -> a -> EvalIO GQLType
    trans (Object gql) = wrapAsObject . transform initMeta gql . from
    trans (Field key) = \x -> IOFail $ subfieldsNotSelected x key

    fieldType :: Proxy a -> Text -> GQL__Field
    default fieldType :: (Show a, Selectors (Rep a) , Typeable a) => Proxy a -> Text -> GQL__Field
    fieldType _ name  = createField name typeName []
        where typeName = (pack . show . typeOf) (undefined::a)

    introspect :: Proxy a -> GQLTypeLib -> GQLTypeLib
    default introspect :: (Show a, Selectors (Rep a) , Typeable a) => Proxy a -> GQLTypeLib -> GQLTypeLib
    introspect _  typeLib = do
        let typeName = (pack . show . typeOf) (undefined::a)
        case (lookup typeName typeLib) of
            Just _ -> typeLib
            Nothing -> arrayMap (insert typeName (createType typeName gqlFields) typeLib) stack
                where
                    fieldTypes  = getFields (Proxy :: Proxy (Rep a))
                    stack = (map snd fieldTypes)
                    gqlFields = map fst fieldTypes

getType :: (GQLRecord a, GQLArgs p) => (p ::-> a) -> (p ::-> a)
getType _ = TypeHolder Nothing

resolveField
    :: (Show a, Show p, GQLRecord a, GQLArgs p)
    => GQLValue
    -> p ::-> a
    -> p ::-> a
    -> EvalIO GQLType
resolveField (Query gqlArgs body) (TypeHolder args) (Resolver resolver) =
    case fromArgs gqlArgs args of
        Val  args -> resolver args >>= trans body
        Fail x    -> IOFail x
resolveField (Query gqlArgs body) _ (Some x) = trans body x
resolveField (Query gqlArgs body) _ None =
    case handleError "resolver not implemented" of
        Fail x -> IOFail x
resolveField field (TypeHolder args) (Resolver resolver) =
    case fromArgs Empty args of
        Val  args -> resolver args >>= trans field
        Fail x    -> IOFail x


instance (Show a, Show p, GQLRecord a , GQLArgs p ) => GQLRecord (p ::-> a) where
    trans (Query args body ) field = resolveField (Query args body) (getType field) field
    trans x (Resolver f) = resolveField x (getType (Resolver f)) (Resolver f)
    trans x (Some a) = trans x a
    trans x None = pure $ Prim JSNull
    introspect  _  = introspect (Proxy:: Proxy  a)
    fieldType _ name = (fieldType (Proxy:: Proxy  a) name ){ args = argsMeta (Proxy :: Proxy p) }

instance (Show a, GQLRecord a) => GQLRecord (Maybe a) where
    trans _ Nothing = pure $ Prim JSNull
    trans query (Just value) = trans query value
    introspect  _ = introspect (Proxy:: Proxy  a)
    fieldType _ = fieldType (Proxy:: Proxy  a)

instance GQLRecord Int where
    trans _ =  pure . Prim . JSInt
    introspect _ = insert "Int" (createType "Int" [])
    fieldType _ name =  createField name "Int" []

instance GQLRecord Text where
    trans _ =  pure . Prim . JSString
    introspect _ = insert "String" (createType "String" [])
    fieldType _  name =  createField name "String" []

instance GQLRecord Bool where
    trans _ =  pure . Prim . JSBool
    introspect _ = insert "Boolean" (createType "Boolean" [])
    fieldType _ name = createField name "Boolean" []

instance GQLRecord a => GQLRecord [a] where
    trans (Field _) x =  pure $ Li []
    trans query list = Li <$> mapM (trans query) list
    introspect _ = introspect (Proxy :: Proxy  a)
    fieldType _ = fieldType (Proxy :: Proxy  a)

instance GQLRecord GQL__EnumValue;

instance GQLRecord GQL__Type;

instance GQLRecord GQL__Field;

instance GQLRecord GQL__InputValue;

instance GQLRecord GQL__Schema;

instance GQLRecord GQL__Directive;

instance GQLRecord GQL__TypeKind where
    introspect _ = insert "__TypeKind" (createType "__TypeKind" [])
    fieldType _ name = createField name "__TypeKind" []
    trans _ = pure . Prim . JSString . pack . show

instance GQLRecord GQL__DirectiveLocation where
    introspect _  = insert "__DirectiveLocation" (createType "__DirectiveLocation" [])
    fieldType _ name = createField name "__DirectiveLocation" []
    trans _ = pure . Prim . JSString . pack . show

instance  GQLArgs GQL__Deprication__Args;

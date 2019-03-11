{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Morpheus.Generics.GQLSelection
  ( GQLSelection(..)
  ) where

import           Control.Monad.Trans.Except
import qualified Data.Data                              as D
import qualified Data.Map                               as M
import qualified Data.Morpheus.ErrorMessage             as Err
import           Data.Morpheus.Generics.DeriveResolvers (DeriveResolvers (..), resolveBySelection)
import qualified Data.Morpheus.Generics.GQLArgs         as Args (GQLArgs (..))
import qualified Data.Morpheus.Generics.GQLEnum         as E (GQLEnum (..))
import           Data.Morpheus.Generics.TypeRep         (Selectors (..), resolveTypes)
import           Data.Morpheus.Schema.GQL__Directive    (GQL__Directive)
import qualified Data.Morpheus.Schema.GQL__Field        as F (GQL__Field (..), createFieldWith)
import           Data.Morpheus.Schema.GQL__Schema       (GQL__Schema)
import           Data.Morpheus.Schema.SchemaField       (wrapAsListType)
import           Data.Morpheus.Types.Introspection      (GQLTypeLib, GQL__EnumValue, GQL__Field,
                                                         GQL__InputValue, GQL__Type, createField,
                                                         createScalar, createType)
import           Data.Morpheus.Types.JSType             (JSType (..))
import qualified Data.Morpheus.Types.MetaInfo           as Meta (MetaInfo (..), initialMeta)
import           Data.Morpheus.Types.Types              ((::->) (..), EnumOf (..),
                                                         QuerySelection (..), ResolveIO,
                                                         failResolveIO)
import           Data.Proxy
import qualified Data.Text                              as T
import           GHC.Generics

instance GQLSelection a => DeriveResolvers (K1 i a) where
  deriveResolvers meta (K1 src) = [(Meta.key meta, (`encode` src))]

instance (Selector s, D.Typeable a, GQLSelection a) => Selectors (M1 S s (K1 R a)) GQL__Field where
  getFields _ = [(fieldType (Proxy :: Proxy a) name, introspect (Proxy :: Proxy a))]
    where
      name = T.pack $ selName (undefined :: M1 S s (K1 R a) ())

class GQLSelection a where
  encode :: QuerySelection -> a -> ResolveIO JSType
  default encode :: (Generic a, D.Data a, DeriveResolvers (Rep a), Show a) =>
    QuerySelection -> a -> ResolveIO JSType
  encode (SelectionSet _ selection _pos) = resolveBySelection selection . deriveResolvers Meta.initialMeta . from
  encode (Field _ key pos) = \_ -> failResolveIO $ Err.subfieldsNotSelected meta
    where
      meta = Meta.MetaInfo {Meta.typeName = "", Meta.key = key, Meta.position = pos}
  typeID :: Proxy a -> T.Text
  default typeID :: (D.Typeable a) =>
    Proxy a -> T.Text
  typeID _ = (T.pack . show . D.typeOf) (undefined :: a)
  fieldType :: Proxy a -> T.Text -> GQL__Field
  default fieldType :: (Show a, Selectors (Rep a) GQL__Field, D.Typeable a) =>
    Proxy a -> T.Text -> GQL__Field
  fieldType proxy name = createField name typeName []
    where
      typeName = typeID proxy
  introspect :: Proxy a -> GQLTypeLib -> GQLTypeLib
  default introspect :: (Show a, Selectors (Rep a) GQL__Field, D.Typeable a) =>
    Proxy a -> GQLTypeLib -> GQLTypeLib
  introspect proxy typeLib =
    case M.lookup typeName typeLib of
      Just _  -> typeLib
      Nothing -> resolveTypes (M.insert typeName (createType typeName gqlFields) typeLib) stack
    where
      typeName = typeID proxy
      fieldTypes = getFields (Proxy :: Proxy (Rep a))
      stack = map snd fieldTypes
      gqlFields = map fst fieldTypes

getType :: (GQLSelection a, Args.GQLArgs p) => (p ::-> a) -> (p ::-> a)
getType _ = TypeHolder Nothing

resolve ::
     (Show a, Show p, GQLSelection a, Args.GQLArgs p) => QuerySelection -> p ::-> a -> p ::-> a -> ResolveIO JSType
resolve (SelectionSet gqlArgs body pos) (TypeHolder args) (Resolver resolver) =
  (ExceptT $ pure $ Args.decode gqlArgs args) >>= resolver >>= encode (SelectionSet gqlArgs body pos)
resolve (Field gqlArgs field pos) (TypeHolder args) (Resolver resolver) =
  (ExceptT $ pure $ Args.decode gqlArgs args) >>= resolver >>= encode (Field gqlArgs field pos)
resolve query _ (Some value) = encode query value
resolve _ _ None = ExceptT $ pure $ Err.handleError "resolver not implemented"

instance (Show a, Show p, GQLSelection a, Args.GQLArgs p, D.Typeable (p ::-> a)) => GQLSelection (p ::-> a) where
  encode (SelectionSet args body pos) field = resolve (SelectionSet args body pos) (getType field) field
  encode (Field args body pos) field = resolve (Field args body pos) (getType field) field
  encode x (Resolver f) = resolve x (getType (Resolver f)) (Resolver f)
  encode x (Some a) = encode x a
  encode _ None = pure JSNull
  introspect _ typeLib = resolveTypes typeLib $ args ++ fields
    where
      args = map snd $ Args.introspect (Proxy :: Proxy p)
      fields = [introspect (Proxy :: Proxy a)]
  fieldType _ name = (fieldType (Proxy :: Proxy a) name) {F.args = map fst $ Args.introspect (Proxy :: Proxy p)}

instance (Show a, GQLSelection a, D.Typeable a) => GQLSelection (Maybe a) where
  encode _ Nothing          = pure JSNull
  encode query (Just value) = encode query value
  introspect _ = introspect (Proxy :: Proxy a)
  fieldType _ = fieldType (Proxy :: Proxy a)

instance GQLSelection Int where
  encode _ = pure . JSInt
  introspect _ = M.insert "Int" $ createScalar "Int"
  fieldType _ name = F.createFieldWith name (createScalar "Int") []

instance GQLSelection T.Text where
  encode _ = pure . JSString
  introspect _ = M.insert "String" $ createScalar "String"
  fieldType _ name = F.createFieldWith name (createScalar "String") []

instance GQLSelection Bool where
  encode _ = pure . JSBool
  introspect _ = M.insert "Boolean" $ createScalar "Boolean"
  fieldType _ name = F.createFieldWith name (createScalar "Boolean") []

instance (GQLSelection a, D.Typeable a) => GQLSelection [a] where
  encode Field {} _ = pure $ JSList []
  encode query list = JSList <$> mapM (encode query) list
  introspect _ = introspect (Proxy :: Proxy a)
  fieldType _ = wrapAsListType <$> fieldType (Proxy :: Proxy a)

instance (Show a, E.GQLEnum a, D.Typeable a) => GQLSelection (EnumOf a) where
  encode _ = pure . JSString . T.pack . show . unpackEnum
  fieldType _ = E.fieldType (Proxy :: Proxy a)
  introspect _ = E.introspect (Proxy :: Proxy a)

instance GQLSelection GQL__EnumValue where
  typeID _ = "__EnumValue"

instance GQLSelection GQL__Type where
  typeID _ = "__Type"

instance GQLSelection GQL__Field where
  typeID _ = "__Field"

instance GQLSelection GQL__InputValue where
  typeID _ = "__InputValue"

instance GQLSelection GQL__Schema where
  typeID _ = "__Schema"

instance GQLSelection GQL__Directive where
  typeID _ = "__Directive"

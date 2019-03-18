{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Morpheus.Generics.GQLSelection
  ( GQLSelection(..)
  ) where

import           Control.Monad.Trans.Except
import qualified Data.Data                              as D
import qualified Data.Map                               as M
import           Data.Morpheus.Error.Internal           (internalUndefinedResolver)
import           Data.Morpheus.Error.Selection          (subfieldsNotSelected)
import           Data.Morpheus.Generics.DeriveResolvers (DeriveResolvers (..), resolveBySelection)
import qualified Data.Morpheus.Generics.GQLArgs         as Args (GQLArgs (..))
import qualified Data.Morpheus.Generics.GQLEnum         as E (GQLEnum (..))
import           Data.Morpheus.Generics.TypeRep         (Selectors (..), resolveTypes)
import           Data.Morpheus.Generics.Utils           (RecSel, SelOf, typeOf)
import           Data.Morpheus.Schema.Directive         (Directive)
import           Data.Morpheus.Schema.EnumValue         (EnumValue)
import qualified Data.Morpheus.Schema.Field             as F (Field (..), createFieldWith)
import           Data.Morpheus.Schema.Schema            (Schema)
import           Data.Morpheus.Schema.Utils.Field       (wrapAsListType)
import           Data.Morpheus.Schema.Utils.Utils       (Field, InputValue, Type, TypeLib,
                                                         createField, createScalar, createType)
import           Data.Morpheus.Types.Describer          ((::->) (..), EnumOf (..))
import           Data.Morpheus.Types.Error              (ResolveIO, failResolveIO)
import           Data.Morpheus.Types.JSType             (JSType (..))
import qualified Data.Morpheus.Types.MetaInfo           as Meta (MetaInfo (..), initialMeta)
import           Data.Morpheus.Types.Types              (QuerySelection (..))
import           Data.Proxy
import qualified Data.Text                              as T
import           GHC.Generics

instance GQLSelection a => DeriveResolvers (K1 i a) where
  deriveResolvers meta (K1 src) = [(Meta.key meta, (`encode` src))]

instance (Selector s, D.Typeable a, GQLSelection a) => Selectors (RecSel s a) Field where
  getFields _ = [(fieldType (Proxy :: Proxy a) name, introspect (Proxy :: Proxy a))]
    where
      name = T.pack $ selName (undefined :: SelOf s)

class GQLSelection a where
  encode :: QuerySelection -> a -> ResolveIO JSType
  default encode :: (Generic a, D.Data a, DeriveResolvers (Rep a), Show a) =>
    QuerySelection -> a -> ResolveIO JSType
  encode (SelectionSet _ selection _pos) = resolveBySelection selection . deriveResolvers Meta.initialMeta . from
  encode (Field _ key pos) = \_ -> failResolveIO $ subfieldsNotSelected meta
    where
      meta = Meta.MetaInfo {Meta.typeName = "", Meta.key = key, Meta.position = pos}
  typeID :: Proxy a -> T.Text
  default typeID :: (D.Typeable a) =>
    Proxy a -> T.Text
  typeID _ = typeOf $ Proxy @a
  fieldType :: Proxy a -> T.Text -> Field
  default fieldType :: (Show a, Selectors (Rep a) Field, D.Typeable a) =>
    Proxy a -> T.Text -> Field
  fieldType proxy name = createField name typeName []
    where
      typeName = typeID proxy
  introspect :: Proxy a -> TypeLib -> TypeLib
  default introspect :: (Show a, Selectors (Rep a) Field, D.Typeable a) =>
    Proxy a -> TypeLib -> TypeLib
  introspect proxy typeLib =
    case M.lookup typeName typeLib of
      Just _  -> typeLib
      Nothing -> resolveTypes (M.insert typeName (createType typeName gqlFields) typeLib) stack
    where
      typeName = typeID proxy
      fieldTypes = getFields (Proxy :: Proxy (Rep a))
      stack = map snd fieldTypes
      gqlFields = map fst fieldTypes

resolve :: (Show a, Show p, GQLSelection a, Args.GQLArgs p) => QuerySelection -> p ::-> a -> ResolveIO JSType
resolve (SelectionSet gqlArgs body pos) (Resolver resolver) =
  (ExceptT $ pure $ Args.decode gqlArgs) >>= resolver >>= encode (SelectionSet gqlArgs body pos)
resolve (Field gqlArgs field pos) (Resolver resolver) =
  (ExceptT $ pure $ Args.decode gqlArgs) >>= resolver >>= encode (Field gqlArgs field pos)
resolve query (Some value) = encode query value
resolve _ None = ExceptT $ pure $ internalUndefinedResolver "resolver not implemented"

instance (Show a, Show p, GQLSelection a, Args.GQLArgs p, D.Typeable (p ::-> a)) => GQLSelection (p ::-> a) where
  encode = resolve
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

instance GQLSelection EnumValue where
  typeID _ = "__EnumValue"

instance GQLSelection Type where
  typeID _ = "__Type"

instance GQLSelection Field where
  typeID _ = "__Field"

instance GQLSelection InputValue where
  typeID _ = "__InputValue"

instance GQLSelection Schema where
  typeID _ = "__Schema"

instance GQLSelection Directive where
  typeID _ = "__Directive"

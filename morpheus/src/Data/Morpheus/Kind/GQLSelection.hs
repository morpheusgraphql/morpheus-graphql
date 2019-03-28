{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Morpheus.Kind.GQLSelection
  ( GQLSelection(..)
  ) where

import           Control.Monad.Trans.Except
import qualified Data.Data                              as D
import qualified Data.Map                               as M
import           Data.Morpheus.Error.Selection          (subfieldsNotSelected)
import           Data.Morpheus.Generics.DeriveResolvers (DeriveResolvers (..), resolveBySelection)
import           Data.Morpheus.Generics.TypeRep         (Selectors (..), resolveTypes)
import           Data.Morpheus.Generics.Utils           (RecSel, SelOf)
import qualified Data.Morpheus.Kind.GQLArgs             as Args (GQLArgs (..))
import qualified Data.Morpheus.Kind.GQLEnum             as E (GQLEnum (..))
import           Data.Morpheus.Kind.GQLKind             (GQLKind (..), asObjectType, scalarTypeOf)
import qualified Data.Morpheus.Kind.Scalar              as S (Scalar (..))
import           Data.Morpheus.Schema.Directive         (Directive)
import           Data.Morpheus.Schema.EnumValue         (EnumValue)
import qualified Data.Morpheus.Schema.Field             as F (Field (..), createFieldWith)
import           Data.Morpheus.Schema.Schema            (Schema)
import           Data.Morpheus.Schema.Type              (DeprecationArgs)
import           Data.Morpheus.Schema.Utils.Field       (wrapAsListType)
import           Data.Morpheus.Schema.Utils.Utils       (Field, InputValue, Type, TypeLib, createField)
import           Data.Morpheus.Types.Describer          ((::->) (..), EnumOf (..), ScalarOf (..),
                                                         WithDeprecationArgs (..))
import           Data.Morpheus.Types.Error              (ResolveIO, failResolveIO)
import           Data.Morpheus.Types.JSType             (JSType (..), Scalar (..))
import qualified Data.Morpheus.Types.MetaInfo           as Meta (MetaInfo (..), initialMeta)
import           Data.Morpheus.Types.Query.Selection    (Selection (..))
import           Data.Proxy
import qualified Data.Text                              as T
import           GHC.Generics

instance GQLSelection a => DeriveResolvers (K1 i a) where
  deriveResolvers meta (K1 src) = [(Meta.key meta, (`encode` src))]

instance (Selector s, D.Typeable a, GQLSelection a) => Selectors (RecSel s a) Field where
  getFields _ = [(fieldType (Proxy @a) name, introspect (Proxy @a))]
    where
      name = T.pack $ selName (undefined :: SelOf s)

class GQLSelection a where
  encode :: Selection -> a -> ResolveIO JSType
  default encode :: (Generic a, D.Data a, DeriveResolvers (Rep a), Show a) =>
    Selection -> a -> ResolveIO JSType
  encode (SelectionSet _ selection _pos) = resolveBySelection selection . deriveResolvers Meta.initialMeta . from
  encode (Field _ key pos) = \_ -> failResolveIO $ subfieldsNotSelected meta -- TODO: must be internal Error
    where
      meta = Meta.MetaInfo {Meta.typeName = "", Meta.key = key, Meta.position = pos}
  fieldType :: Proxy a -> T.Text -> Field
  default fieldType :: (Show a, Selectors (Rep a) Field, D.Typeable a, GQLKind a) =>
    Proxy a -> T.Text -> Field
  fieldType proxy name = createField name typeName []
    where
      typeName = typeID proxy
  introspect :: Proxy a -> TypeLib -> TypeLib
  default introspect :: (Show a, Selectors (Rep a) Field, GQLKind a) =>
    Proxy a -> TypeLib -> TypeLib
  introspect = updateLib (asObjectType fields) stack
    where
      fieldTypes = getFields (Proxy @(Rep a))
      fields = map fst fieldTypes
      stack = map snd fieldTypes

instance (GQLSelection a, Args.GQLArgs p) => GQLSelection (p ::-> a) where
  encode (SelectionSet gqlArgs body pos) (Resolver resolver) =
    (ExceptT $ pure $ Args.decode gqlArgs) >>= resolver >>= encode (SelectionSet gqlArgs body pos)
  encode (Field gqlArgs field pos) (Resolver resolver) =
    (ExceptT $ pure $ Args.decode gqlArgs) >>= resolver >>= encode (Field gqlArgs field pos)
  introspect _ typeLib = resolveTypes typeLib $ args ++ fields
    where
      args = map snd $ Args.introspect (Proxy @p)
      fields = [introspect (Proxy @a)]
  fieldType _ name = (fieldType (Proxy @a) name) {F.args = map fst $ Args.introspect (Proxy @p)}

-- manual deriving of  DeprecationArgs ::-> a
instance GQLSelection a => GQLSelection (WithDeprecationArgs a) where
  encode sel (WithDeprecationArgs val) = encode sel val
  introspect _ typeLib = resolveTypes typeLib $ args ++ fields
    where
      args = map snd $ Args.introspect (Proxy @DeprecationArgs)
      fields = [introspect (Proxy @a)]
  fieldType _ name = (fieldType (Proxy @a) name) {F.args = map fst $ Args.introspect (Proxy @DeprecationArgs)}

instance GQLSelection a => GQLSelection (Maybe a) where
  encode _ Nothing          = pure JSNull
  encode query (Just value) = encode query value
  introspect _ = introspect (Proxy @a)
  fieldType _ = fieldType (Proxy @a)

introspectScalar :: GQLKind a => Proxy a -> TypeLib -> TypeLib
introspectScalar proxy = M.insert (typeID proxy) (scalarTypeOf proxy)

scalarField :: GQLKind a => Proxy a -> T.Text -> Field
scalarField proxy name = F.createFieldWith name (scalarTypeOf proxy) []

instance GQLSelection Int where
  encode _ = pure . Scalar . Int
  introspect = introspectScalar
  fieldType = scalarField

instance GQLSelection T.Text where
  encode _ = pure . Scalar . String
  introspect = introspectScalar
  fieldType = scalarField

instance GQLSelection Bool where
  encode _ = pure . Scalar . Boolean
  introspect = introspectScalar
  fieldType = scalarField

instance (GQLSelection a, D.Typeable a) => GQLSelection [a] where
  encode Field {} _ = pure $ JSList []
  encode query list = JSList <$> mapM (encode query) list
  introspect _ = introspect (Proxy @a)
  fieldType _ = wrapAsListType <$> fieldType (Proxy @a)

instance (Show a, GQLKind a, E.GQLEnum a) => GQLSelection (EnumOf a) where
  encode _ = pure . Scalar . String . T.pack . show . unpackEnum
  fieldType _ = E.asField (Proxy @a)
  introspect _ = E.introspect (Proxy @a)

instance S.Scalar a => GQLSelection (ScalarOf a) where
  encode _ (ScalarOf x) = pure $ Scalar $ S.serialize x
  fieldType _ = S.asField (Proxy @a)
  introspect _ = S.introspect (Proxy @a)

instance GQLSelection EnumValue

instance GQLSelection Type

instance GQLSelection Field

instance GQLSelection InputValue

instance GQLSelection Schema

instance GQLSelection Directive

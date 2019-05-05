{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Morpheus.Kind.GQLObject
  ( GQLObject(..)
  ) where

import           Control.Monad.Trans                    (lift)
import           Control.Monad.Trans.Except
import           Data.Morpheus.Error.Selection          (fieldNotResolved, subfieldsNotSelected)
import           Data.Morpheus.Generics.DeriveResolvers (DeriveResolvers (..), resolveBySelection)
import           Data.Morpheus.Generics.TypeRep         (Selectors (..), resolveTypes)
import qualified Data.Morpheus.Kind.GQLArgs             as Args (GQLArgs (..))
import           Data.Morpheus.Kind.GQLKind             (GQLKind (..), asObjectType)
import           Data.Morpheus.Kind.Internal            (GQL, GQLConstraint, OBJECT)
import           Data.Morpheus.Kind.Utils               (encodeList, encodeMaybe, listField, nullableField)
import           Data.Morpheus.Schema.Directive         (Directive)
import           Data.Morpheus.Schema.EnumValue         (EnumValue)
import           Data.Morpheus.Schema.Internal.Types    (ObjectField (..), TypeLib)
import qualified Data.Morpheus.Schema.Internal.Types    as I (Field (..))
import           Data.Morpheus.Schema.Schema            (Schema)
import           Data.Morpheus.Schema.Type              (DeprecationArgs)
import           Data.Morpheus.Schema.TypeKind          (TypeKind (..))
import           Data.Morpheus.Schema.Utils.Utils       (Field, InputValue, Type)
import           Data.Morpheus.Types.Describer          ((::->) (..), WithDeprecationArgs (..))
import           Data.Morpheus.Types.Error              (ResolveIO, failResolveIO)
import           Data.Morpheus.Types.JSType             (JSType (..))
import qualified Data.Morpheus.Types.MetaInfo           as Meta (MetaInfo (..), initialMeta)
import           Data.Morpheus.Types.Query.Selection    (Selection (..))
import           Data.Proxy
import           Data.Text                              (Text, pack)
import           GHC.Generics

class GQLObject a where
  encode :: (Text, Selection) -> a -> ResolveIO JSType
  default encode :: (Generic a, DeriveResolvers (Rep a)) =>
    (Text, Selection) -> a -> ResolveIO JSType
  encode (_, SelectionSet _ selection _pos) = resolveBySelection selection . deriveResolvers Meta.initialMeta . from
  encode (_, Field _ key pos) = const $ failResolveIO $ subfieldsNotSelected meta -- TODO: must be internal Error
    where
      meta = Meta.MetaInfo {Meta.typeName = "", Meta.key = key, Meta.position = pos}
  fieldType :: Proxy a -> Text -> ObjectField
  default fieldType :: (Selectors (Rep a) (Text, ObjectField), GQLKind a) =>
    Proxy a -> Text -> ObjectField
  fieldType proxy name =
    ObjectField [] $
    I.Field {I.fieldName = name, I.notNull = True, I.asList = False, I.kind = OBJECT, I.fieldType = typeID proxy}
  introspect :: Proxy a -> TypeLib -> TypeLib
  default introspect :: (Selectors (Rep a) (Text, ObjectField), GQLKind a) =>
    Proxy a -> TypeLib -> TypeLib
  introspect = updateLib (asObjectType fields) stack
    where
      fieldTypes = getFields (Proxy @(Rep a))
      fields = map fst fieldTypes
      stack = map snd fieldTypes

liftResolver :: Int -> Text -> IO (Either String a) -> ResolveIO a
liftResolver position' typeName' x = do
  result <- lift x
  case result of
    Left message' -> failResolveIO $ fieldNotResolved position' typeName' (pack message')
    Right value   -> pure value

instance (GQLObject a, Args.GQLArgs p) => GQLObject (p ::-> a) where
  encode (key', SelectionSet gqlArgs body position') (Resolver resolver) =
    (ExceptT $ pure $ Args.decode gqlArgs) >>= liftResolver position' key' . resolver >>=
    encode (key', SelectionSet gqlArgs body position')
  encode (key', Field gqlArgs field position') (Resolver resolver) =
    (ExceptT $ pure $ Args.decode gqlArgs) >>= liftResolver position' key' . resolver >>=
    encode (key', Field gqlArgs field position')
  introspect _ typeLib = resolveTypes typeLib $ args' ++ fields
    where
      args' = map snd $ Args.introspect (Proxy @p)
      fields = [introspect (Proxy @a)]
  fieldType _ name = (fieldType (Proxy @a) name) {args = map fst $ Args.introspect (Proxy @p)}

-- manual deriving of  DeprecationArgs ::-> a
instance GQLObject a => GQLObject (WithDeprecationArgs a) where
  encode sel (WithDeprecationArgs val) = encode sel val
  introspect _ typeLib = resolveTypes typeLib $ args' ++ fields
    where
      args' = map snd $ Args.introspect (Proxy @DeprecationArgs)
      fields = [introspect (Proxy @a)]
  fieldType _ name = (fieldType (Proxy @a) name) {args = map fst $ Args.introspect (Proxy @DeprecationArgs)}

instance GQLObject a => GQLObject (Maybe a) where
  encode = encodeMaybe encode
  introspect _ = introspect (Proxy @a)
  fieldType _ name = nullableField (fieldType (Proxy @a) name)

instance GQLObject a => GQLObject [a] where
  encode = encodeList encode
  introspect _ = introspect (Proxy @a)
  fieldType _ name = listField (fieldType (Proxy @a) name)

instance GQLObject EnumValue

instance GQLObject Type

instance GQLObject Field

instance GQLObject InputValue

instance GQLObject Schema

instance GQLObject Directive

type instance GQLConstraint a OBJECT = (GQLObject a, GQLKind a)

type instance GQL EnumValue = OBJECT

type instance GQL Type = OBJECT

type instance GQL Field = OBJECT

type instance GQL InputValue = OBJECT

type instance GQL Schema = OBJECT

type instance GQL Directive = OBJECT

type instance GQL (WithDeprecationArgs a) = GQL a

type instance GQL (p ::-> a) = GQL a

{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Morpheus.Kind.GQLObject
  ( GQLObject(..)
  ) where

import           Control.Monad.Trans                    (lift)
import           Control.Monad.Trans.Except
import qualified Data.Data                              as D
import           Data.Morpheus.Error.Selection          (fieldNotResolved, subfieldsNotSelected)
import           Data.Morpheus.Generics.DeriveResolvers (DeriveResolvers (..), resolveBySelection)
import           Data.Morpheus.Generics.TypeRep         (Selectors (..), resolveTypes)
import           Data.Morpheus.Generics.Utils           (RecSel, SelOf)
import qualified Data.Morpheus.Kind.GQLArgs             as Args (GQLArgs (..))
import qualified Data.Morpheus.Kind.GQLEnum             as E (GQLEnum (..))
import           Data.Morpheus.Kind.GQLKind             (GQLKind (..), asObjectType, introspectScalar)
import qualified Data.Morpheus.Kind.GQLScalar           as S (GQLScalar (..))
import           Data.Morpheus.Schema.Directive         (Directive)
import           Data.Morpheus.Schema.EnumValue         (EnumValue)
import           Data.Morpheus.Schema.Internal.Types    (ObjectField (..), TypeLib)
import qualified Data.Morpheus.Schema.Internal.Types    as I (Field (..))
import           Data.Morpheus.Schema.Schema            (Schema)
import           Data.Morpheus.Schema.Type              (DeprecationArgs)
import           Data.Morpheus.Schema.TypeKind          (TypeKind (..))
import           Data.Morpheus.Schema.Utils.Utils       (Field, InputValue, Type)
import           Data.Morpheus.Types.Describer          ((::->) (..), EnumOf (..), ScalarOf (..),
                                                         WithDeprecationArgs (..))
import           Data.Morpheus.Types.Error              (ResolveIO, failResolveIO)
import           Data.Morpheus.Types.JSType             (JSType (..), ScalarValue (..))
import qualified Data.Morpheus.Types.MetaInfo           as Meta (MetaInfo (..), initialMeta)
import           Data.Morpheus.Types.Query.Selection    (Selection (..))
import           Data.Proxy
import           Data.Text                              (Text, pack)
import           GHC.Generics

instance GQLObject a => DeriveResolvers (K1 i a) where
  deriveResolvers meta (K1 src) = [(Meta.key meta, (`encode` src))]

instance (Selector s, D.Typeable a, GQLObject a) => Selectors (RecSel s a) (Text, ObjectField) where
  getFields _ = [((name, fieldType (Proxy @a) name), introspect (Proxy @a))]
    where
      name = pack $ selName (undefined :: SelOf s)

class GQLObject a where
  encode :: (Text, Selection) -> a -> ResolveIO JSType
  default encode :: (Generic a, D.Data a, DeriveResolvers (Rep a), Show a) =>
    (Text, Selection) -> a -> ResolveIO JSType
  encode (_, SelectionSet _ selection _pos) = resolveBySelection selection . deriveResolvers Meta.initialMeta . from
  encode (_, Field _ key pos) = const $ failResolveIO $ subfieldsNotSelected meta -- TODO: must be internal Error
    where
      meta = Meta.MetaInfo {Meta.typeName = "", Meta.key = key, Meta.position = pos}
  fieldType :: Proxy a -> Text -> ObjectField
  default fieldType :: (Show a, Selectors (Rep a) (Text, ObjectField), D.Typeable a, GQLKind a) =>
    Proxy a -> Text -> ObjectField
  fieldType proxy name =
    ObjectField [] $
    I.Field {I.fieldName = name, I.notNull = True, I.asList = False, I.kind = OBJECT, I.fieldType = typeID proxy}
  introspect :: Proxy a -> TypeLib -> TypeLib
  default introspect :: (Show a, Selectors (Rep a) (Text, ObjectField), GQLKind a) =>
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
  encode _ Nothing          = pure JSNull
  encode query (Just value) = encode query value
  introspect _ = introspect (Proxy @a)
  fieldType _ name = (fType name) {fieldContent = (fieldContent $ fType name) {I.notNull = False}}
    where
      fType = fieldType (Proxy @a)

scalarField :: GQLKind a => Proxy a -> Text -> ObjectField
scalarField proxy name =
  ObjectField
    []
    I.Field {I.fieldName = name, I.notNull = True, I.asList = False, I.kind = SCALAR, I.fieldType = typeID proxy}

instance GQLObject Int where
  encode _ = pure . Scalar . Int
  introspect = introspectScalar
  fieldType = scalarField

instance GQLObject Float where
  encode _ = pure . Scalar . Float
  introspect = introspectScalar
  fieldType = scalarField

instance GQLObject Text where
  encode _ = pure . Scalar . String
  introspect = introspectScalar
  fieldType = scalarField

instance GQLObject Bool where
  encode _ = pure . Scalar . Boolean
  introspect = introspectScalar
  fieldType = scalarField

instance (GQLObject a, D.Typeable a) => GQLObject [a] where
  encode (_, Field {}) _ = pure $ JSList []
  encode query list      = JSList <$> mapM (encode query) list
  introspect _ = introspect (Proxy @a)
  fieldType _ name = fType {fieldContent = (fieldContent fType) {I.asList = True}}
    where
      fType = fieldType (Proxy @a) name

instance (Show a, GQLKind a, E.GQLEnum a) => GQLObject (EnumOf a) where
  encode _ = pure . Scalar . String . pack . show . unpackEnum
  fieldType _ = ObjectField [] . E.asField (Proxy @a)
  introspect _ = E.introspect (Proxy @a)

instance S.GQLScalar a => GQLObject (ScalarOf a) where
  encode _ (ScalarOf x) = pure $ Scalar $ S.serialize x
  fieldType _ = ObjectField [] . S.asField (Proxy @a)
  introspect _ = S.introspect (Proxy @a)

instance GQLObject EnumValue

instance GQLObject Type

instance GQLObject Field

instance GQLObject InputValue

instance GQLObject Schema

instance GQLObject Directive

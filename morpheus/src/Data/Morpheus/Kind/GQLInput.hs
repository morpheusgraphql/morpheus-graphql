{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Morpheus.Kind.GQLInput
  ( GQLInput(..)
  ) where

import           Data.Morpheus.Error.Internal        (internalArgumentError, internalTypeMismatch)
import           Data.Morpheus.Generics.GDecode      (GDecode (..))
import           Data.Morpheus.Generics.TypeRep      (Selectors (..))
import qualified Data.Morpheus.Kind.GQLEnum          as E (GQLEnum (..))
import           Data.Morpheus.Kind.GQLKind          (GQLKind (..), inputObjectOf)
import           Data.Morpheus.Kind.GQLPrimitive     (GQLPrimitive (..))
import qualified Data.Morpheus.Kind.GQLScalar        as S (GQLScalar (..))
import           Data.Morpheus.Kind.Internal
import           Data.Morpheus.Schema.Internal.Types (Field (..), InputField (..), ObjectField (..), TypeLib)
import           Data.Morpheus.Schema.TypeKind       (TypeKind (..))
import           Data.Morpheus.Types.Error           (Validation)
import           Data.Morpheus.Types.JSType          (JSType (..), ScalarValue (..))
import qualified Data.Morpheus.Types.MetaInfo        as Meta (MetaInfo (..), initialMeta)
import           Data.Proxy                          (Proxy (..))
import           Data.Text                           (Text, pack)
import           GHC.Generics

instance (IntrospectionRouter a (GQL a)) => GDecode JSType (K1 i a) where
  gDecode meta (JSObject object) =
    case lookup (Meta.key meta) object of
      Nothing    -> internalArgumentError "Missing Argument"
      Just value -> K1 <$> _decode value
  gDecode _ isType = internalTypeMismatch "InputObject" isType

class GQLInput a where
  decode :: JSType -> Validation a
  default decode :: (Generic a, GDecode JSType (Rep a)) =>
    JSType -> Validation a
  decode (JSObject x) = to <$> gDecode Meta.initialMeta (JSObject x)
  decode isType       = internalTypeMismatch "InputObject" isType
  asArgument :: Proxy a -> Text -> InputField
  default asArgument :: GQLKind a =>
    Proxy a -> Text -> InputField
  asArgument proxy name =
    InputField $ Field {fieldName = name, notNull = True, asList = False, kind = INPUT_OBJECT, fieldType = typeID proxy}
  introInput :: Proxy a -> TypeLib -> TypeLib
  default introInput :: (GQLKind a, Selectors (Rep a) (Text, InputField)) =>
    Proxy a -> TypeLib -> TypeLib
  introInput = updateLib (inputObjectOf fields) stack
    where
      fieldTypes = getFields (Proxy @(Rep a))
      stack = map snd fieldTypes
      fields = map fst fieldTypes

setNullable :: Field -> Field
setNullable x = x {notNull = False}

wrapMaybe :: InputField -> InputField
wrapMaybe = InputField . setNullable . unpackInputField

instance IntrospectionRouter a (GQL a) => GQLInput (Maybe a) where
  decode JSNull = pure Nothing
  decode x      = Just <$> _decode x
  asArgument _ name = wrapMaybe $ _field (Proxy @a) name
  introInput _ = _introspect (Proxy @a)

instance (GQLInput a, GQLKind a) => GQLInput [a] where
  decode (JSList li) = mapM decode li
  decode isType      = internalTypeMismatch "List" isType
  asArgument _ name = fType {unpackInputField = (unpackInputField fType) {asList = True}}
    where
      fType = asArgument (Proxy @a) name
  introInput _ = introInput (Proxy @a)

type instance GQLConstraint a SCALAR = S.GQLScalar a

type instance GQLConstraint a ENUM = E.GQLEnum a

type instance GQLConstraint a INPUT_OBJECT = GQLInput a

instance (S.GQLScalar a, GQLKind a) => IntrospectionRouter a SCALAR where
  __decode _ = S.decode
  __introspect _ _ = S.introspect (Proxy @a)
  __field _ _ = S.asInputField (Proxy @a)
  __encode _ _ = pure . S.encode
  __objectField _ _ = ObjectField [] . S.asField (Proxy @a)

instance (E.GQLEnum a, Show a, GQLKind a) => IntrospectionRouter a ENUM where
  __decode _ (JSEnum value) = pure (E.decode value)
  __decode _ isType         = internalTypeMismatch "Enum" isType
  __introspect _ _ = E.introspect (Proxy @a)
  __field _ _ = E.asInputField (Proxy @a)
  __encode _ _ = pure . Scalar . String . pack . show
  __objectField _ _ = ObjectField [] . E.asField (Proxy @a)

instance (GQLInput a, GQLKind a) => IntrospectionRouter a INPUT_OBJECT where
  __decode _ = decode
  __introspect _ = introInput
  __field _ = asArgument

instance (GQLPrimitive a, GQLKind a) => IntrospectionRouter a PRIMITIVE where
  __encode _ = encode'
  __decode _ = decode'
  __field _ = inputField'
  __introspect _ = introspect'
  __objectField _ = objectField'

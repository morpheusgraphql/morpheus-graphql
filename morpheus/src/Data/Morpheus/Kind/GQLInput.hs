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
import           Data.Morpheus.Kind.GQLKind          (GQLKind (..), inputObjectOf, introspectScalar)
import qualified Data.Morpheus.Kind.GQLScalar        as S (GQLScalar (..))
import           Data.Morpheus.Kind.Internal
import           Data.Morpheus.Schema.Internal.Types (Field (..), InputField (..), TypeLib)
import           Data.Morpheus.Schema.TypeKind       (TypeKind (..))
import           Data.Morpheus.Types.Error           (Validation)
import           Data.Morpheus.Types.JSType          (JSType (..), ScalarValue (..))
import qualified Data.Morpheus.Types.MetaInfo        as Meta (MetaInfo (..), initialMeta)
import           Data.Proxy                          (Proxy (..))
import           Data.Text                           (Text)
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

inputFieldOf :: GQLKind a => Proxy a -> Text -> InputField
inputFieldOf proxy name =
  InputField $ Field {fieldName = name, asList = False, notNull = True, kind = SCALAR, fieldType = typeID proxy}

instance GQLInput Text where
  decode (Scalar (String x)) = pure x
  decode isType              = internalTypeMismatch "String" isType
  asArgument = inputFieldOf
  introInput = introspectScalar

instance GQLInput Bool where
  decode (Scalar (Boolean x)) = pure x
  decode isType               = internalTypeMismatch "Boolean" isType
  asArgument = inputFieldOf
  introInput = introspectScalar

instance GQLInput Int where
  decode (Scalar (Int x)) = pure x
  decode isType           = internalTypeMismatch "Int" isType
  asArgument = inputFieldOf
  introInput = introspectScalar

instance GQLInput Float where
  decode (Scalar (Float x)) = pure x
  decode isType             = internalTypeMismatch "Int" isType
  asArgument = inputFieldOf
  introInput = introspectScalar

instance (GQLInput a, GQLKind a) => GQLInput (Maybe a) where
  decode JSNull = pure Nothing
  decode x      = Just <$> decode x
  asArgument _ name = InputField $ setNullable $ unpackInputField $ asArgument (Proxy @a) name
    where
      setNullable :: Field -> Field
      setNullable x = x {notNull = False}
  introInput _ typeLib = typeLib

instance (GQLInput a, GQLKind a) => GQLInput [a] where
  decode (JSList li) = mapM decode li
  decode isType      = internalTypeMismatch "List" isType
  asArgument _ name = fType {unpackInputField = (unpackInputField fType) {asList = True}}
    where
      fType = asArgument (Proxy @a) name
  introInput _ = introInput (Proxy @a)

type instance GQLConstraint a PRIMITIVE = GQLKind a

type instance GQLConstraint a SCALAR = S.GQLScalar a

type instance GQLConstraint a ENUM = E.GQLEnum a

type instance GQLConstraint a INPUT_OBJECT = GQLInput a

instance GQLInput a => IntrospectionRouter a PRIMITIVE where
  __decode _ = decode
  __introspect _ = introInput
  __field _ = asArgument

instance (S.GQLScalar a, GQLKind a) => IntrospectionRouter a SCALAR where
  __decode _ = S.decode
  __introspect _ _ = S.introspect (Proxy @a)
  __field _ _ = S.asInputField (Proxy @a)

instance (E.GQLEnum a, GQLKind a) => IntrospectionRouter a ENUM where
  __decode _ (JSEnum value) = pure (E.decode value)
  __decode _ isType         = internalTypeMismatch "Enum" isType
  __introspect _ _ = E.introspect (Proxy @a)
  __field _ _ = E.asInputField (Proxy @a)

instance (GQLInput a, GQLKind a) => IntrospectionRouter a INPUT_OBJECT where
  __decode _ = decode
  __introspect _ = introInput
  __field _ = asArgument

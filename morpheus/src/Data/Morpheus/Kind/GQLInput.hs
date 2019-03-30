{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Data.Morpheus.Kind.GQLInput
  ( GQLInput(..)
  ) where

import           Data.Morpheus.Error.Internal        (internalArgumentError, internalTypeMismatch)
import           Data.Morpheus.Generics.GDecode      (GDecode (..))
import           Data.Morpheus.Generics.TypeRep      (Selectors (..))
import qualified Data.Morpheus.Kind.GQLEnum          as E (GQLEnum (..))
import           Data.Morpheus.Kind.GQLKind          (GQLKind (..), inputObjectOf)
import qualified Data.Morpheus.Kind.Scalar           as S (Scalar (..))
import           Data.Morpheus.Schema.Internal.Types (Field (..), InputField (..), TypeLib)
import           Data.Morpheus.Schema.TypeKind       (TypeKind (..))
import           Data.Morpheus.Types.Describer       (EnumOf (..), ScalarOf (..))
import           Data.Morpheus.Types.Error           (Validation)
import           Data.Morpheus.Types.JSType          (JSType (..), ScalarValue (..))
import qualified Data.Morpheus.Types.MetaInfo        as Meta (MetaInfo (..), initialMeta)
import           Data.Proxy                          (Proxy (..))
import           Data.Text                           (Text)
import           GHC.Generics

instance GQLInput a => GDecode JSType (K1 i a) where
  gDecode meta (JSObject object) =
    case lookup (Meta.key meta) object of
      Nothing    -> internalArgumentError "Missing Argument"
      Just value -> K1 <$> decode value
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
    InputField $ Field {fieldName = name, notNull = True, kind = INPUT_OBJECT, typeName = typeID proxy}
  introInput :: Proxy a -> TypeLib -> TypeLib
  default introInput :: (GQLKind a, Selectors (Rep a) InputField) =>
    Proxy a -> TypeLib -> TypeLib
  introInput = updateLib (inputObjectOf fields) stack
    where
      fieldTypes = getFields (Proxy @(Rep a))
      stack = map snd fieldTypes
      fields = map fst fieldTypes

inputFieldOf :: GQLKind a => Proxy a -> Text -> InputField
inputFieldOf proxy name = InputField $ Field {fieldName = name, notNull = True, kind = SCALAR, typeName = typeID proxy}

introspectInput :: Proxy a -> TypeLib -> TypeLib
introspectInput _ typeLib = typeLib

instance GQLInput Text where
  decode (Scalar (String x)) = pure x
  decode isType              = internalTypeMismatch "String" isType
  asArgument = inputFieldOf
  introInput = introspectInput

instance GQLInput Bool where
  decode (Scalar (Boolean x)) = pure x
  decode isType               = internalTypeMismatch "Boolean" isType
  asArgument = inputFieldOf
  introInput = introspectInput

instance GQLInput Int where
  decode (Scalar (Int x)) = pure x
  decode isType           = internalTypeMismatch "Int" isType
  asArgument = inputFieldOf
  introInput = introspectInput

instance (GQLInput a, GQLKind a) => GQLInput (Maybe a) where
  decode JSNull = pure Nothing
  decode x      = Just <$> decode x
  asArgument _ name = InputField $ setNullable $ unpackInputField $ asArgument (Proxy @a) name
    where
      setNullable :: Field -> Field
      setNullable x = x {notNull = False}
  introInput _ typeLib = typeLib

instance (E.GQLEnum a, GQLKind a) => GQLInput (EnumOf a) where
  decode (JSEnum text) = pure $ EnumOf (E.decode text)
  decode isType        = internalTypeMismatch "Enum" isType
  asArgument _ = E.asInputField (Proxy @a)
  introInput _ = E.introspect (Proxy @a)

instance (S.Scalar a, GQLKind a) => GQLInput (ScalarOf a) where
  decode text = ScalarOf <$> S.decode text
  asArgument _ = S.asInputField (Proxy @a)
  introInput _ = S.introspect (Proxy @a)

instance (GQLInput a, GQLKind a) => GQLInput [a] where
  decode (JSList li) = mapM decode li
  decode isType      = internalTypeMismatch "List" isType
  asArgument _ = asArgument (Proxy @a)
  introInput _ = introInput (Proxy @a) -- wrap as List

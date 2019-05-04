{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Morpheus.Kind.GQLPrimitive where

import Data.Morpheus.Error.Internal (internalTypeMismatch)
import Data.Morpheus.Kind.GQLKind (GQLKind(..), introspectScalar)
import Data.Morpheus.Kind.Internal
  ( GQL
  , GQLConstraint
  , IntrospectionRouter(..)
  , PRIMITIVE
  , _decode
  , _field
  , _introspect
  )
import Data.Morpheus.Schema.Internal.Types (Field(..), InputField(..), TypeLib)
import Data.Morpheus.Schema.TypeKind (TypeKind(..))
import Data.Morpheus.Types.Error (Validation)
import Data.Morpheus.Types.JSType (JSType(..), ScalarValue(..))
import Data.Proxy (Proxy(..))
import Data.Text (Text)

class GQLPrimitive a where
  decode' :: JSType -> Validation a
  inputField' :: GQLKind a => Proxy a -> Text -> InputField
  inputField' proxy name =
    InputField $ Field {fieldName = name, asList = False, notNull = True, kind = SCALAR, fieldType = typeID proxy}
  introspect' :: GQLKind a => Proxy a -> TypeLib -> TypeLib
  introspect' = introspectScalar

instance GQLPrimitive Text where
  decode' (Scalar (String x)) = pure x
  decode' isType = internalTypeMismatch "String" isType

instance GQLPrimitive Bool where
  decode' (Scalar (Boolean x)) = pure x
  decode' isType = internalTypeMismatch "Boolean" isType

instance GQLPrimitive Int where
  decode' (Scalar (Int x)) = pure x
  decode' isType = internalTypeMismatch "Int" isType

instance GQLPrimitive Float where
  decode' (Scalar (Float x)) = pure x
  decode' isType = internalTypeMismatch "Int" isType

type instance GQLConstraint a PRIMITIVE = GQLKind a

setNullable :: Field -> Field
setNullable x = x {notNull = False}

wrapMaybe :: InputField -> InputField
wrapMaybe = InputField . setNullable . unpackInputField



instance IntrospectionRouter a (GQL a) => GQLPrimitive (Maybe a) where
  decode' JSNull = pure Nothing
  decode' x = Just <$> _decode x
  inputField' _ name = wrapMaybe $ _field (Proxy @a) name
  introspect' _ = _introspect (Proxy @a)

instance IntrospectionRouter a (GQL a) => GQLPrimitive [a] where
  decode' (JSList li) = mapM _decode li
  decode' isType = internalTypeMismatch "List" isType
  inputField' _ name = fType {unpackInputField = (unpackInputField fType) {asList = True}}
    where
      fType = _field (Proxy @a) name
  introspect' _ = _introspect (Proxy @a)
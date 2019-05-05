{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Morpheus.Kind.GQLPrimitive where

import           Data.Morpheus.Error.Internal        (internalTypeMismatch)
import           Data.Morpheus.Kind.GQLKind          (GQLKind (..), introspectScalar)
import           Data.Morpheus.Kind.Internal         (GQLConstraint, PRIMITIVE)
import           Data.Morpheus.Schema.Internal.Types (Field (..), InputField (..), ObjectField (..), TypeLib)
import           Data.Morpheus.Schema.TypeKind       (TypeKind (..))
import           Data.Morpheus.Types.Error           (ResolveIO, Validation)
import           Data.Morpheus.Types.JSType          (JSType (..), ScalarValue (..))
import           Data.Morpheus.Types.Query.Selection (Selection)
import           Data.Proxy                          (Proxy (..))
import           Data.Text                           (Text)

scalarField :: GQLKind a => Proxy a -> Text -> ObjectField
scalarField proxy name =
  ObjectField [] Field {fieldName = name, notNull = True, asList = False, kind = SCALAR, fieldType = typeID proxy}

class GQLPrimitive a where
  encode' :: (Text, Selection) -> a -> ResolveIO JSType
  decode' :: JSType -> Validation a
  inputField' :: GQLKind a => Proxy a -> Text -> InputField
  inputField' proxy name =
    InputField $ Field {fieldName = name, asList = False, notNull = True, kind = SCALAR, fieldType = typeID proxy}
  introspect' :: GQLKind a => Proxy a -> TypeLib -> TypeLib
  introspect' = introspectScalar
  objectField' :: GQLKind a => Proxy a -> Text -> ObjectField
  objectField' = scalarField

instance GQLPrimitive Text where
  encode' _ = pure . Scalar . String
  decode' (Scalar (String x)) = pure x
  decode' isType              = internalTypeMismatch "String" isType

instance GQLPrimitive Bool where
  encode' _ = pure . Scalar . Boolean
  decode' (Scalar (Boolean x)) = pure x
  decode' isType               = internalTypeMismatch "Boolean" isType

instance GQLPrimitive Int where
  encode' _ = pure . Scalar . Int
  decode' (Scalar (Int x)) = pure x
  decode' isType           = internalTypeMismatch "Int" isType

instance GQLPrimitive Float where
  encode' _ = pure . Scalar . Float
  decode' (Scalar (Float x)) = pure x
  decode' isType             = internalTypeMismatch "Int" isType

type instance GQLConstraint a PRIMITIVE = GQLKind a

{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DefaultSignatures       #-}
{-# LANGUAGE OverloadedStrings       #-}

module Data.Morpheus.Kind.GQLScalar where

import           Control.Monad                       ((>=>))
import           Data.Morpheus.Error.Internal        (internalTypeMismatch)
import           Data.Morpheus.Kind.GQLKind          (GQLKind (..), scalarTypeOf)
import           Data.Morpheus.Schema.Internal.Types (Field (..), InputField (..), TypeLib)
import           Data.Morpheus.Schema.TypeKind       (TypeKind (..))
import           Data.Morpheus.Types.Core            (Key)
import           Data.Morpheus.Types.Error           (Validation)
import           Data.Morpheus.Types.JSType          (JSType (..), ScalarValue (..))
import           Data.Proxy                          (Proxy (..))

toScalar :: JSType -> Validation ScalarValue
toScalar (Scalar x) = pure x
toScalar jsType     = internalTypeMismatch "Scalar" jsType

class GQLScalar a where
  parseValue :: ScalarValue -> Validation a
  decode :: JSType -> Validation a
  decode = toScalar >=> parseValue
  serialize :: a -> ScalarValue
  encode :: a -> JSType
  encode = Scalar . serialize
  asInputField :: GQLKind a => Proxy a -> Key -> InputField
  asInputField proxy = InputField . asField proxy
  asField :: Proxy a -> Key -> Field
  default asField :: GQLKind a =>
    Proxy a -> Key -> Field
  asField proxy name = Field {fieldName = name, notNull = True, asList = False, kind = SCALAR, fieldType = typeID proxy}
  introspect :: Proxy a -> TypeLib -> TypeLib
  default introspect :: GQLKind a =>
    Proxy a -> TypeLib -> TypeLib
  introspect = updateLib scalarTypeOf []

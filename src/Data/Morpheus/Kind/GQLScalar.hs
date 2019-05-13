{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}

module Data.Morpheus.Kind.GQLScalar where

import           Control.Monad                       ((>=>))
import           Data.Morpheus.Error.Internal        (internalTypeMismatch)
import           Data.Morpheus.Kind.GQLType          (GQLType (..), scalarTypeOf)
import           Data.Morpheus.Schema.Internal.AST (Field (..), InputField (..), TypeLib)
import           Data.Morpheus.Schema.TypeKind       (TypeKind (..))
import           Data.Morpheus.Types.Core            (Key)
import           Data.Morpheus.Types.Error           (Validation)
import           Data.Morpheus.Types.JSType          (JSType (..), ScalarValue (..))
import           Data.Proxy                          (Proxy (..))
import           Data.Text                           (Text)

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
  asInputField :: GQLType a => Proxy a -> Key -> InputField
  asInputField _ = InputField . asField (Proxy @a)
  asField :: GQLType a => Proxy a -> Key -> Field
  asField _ = field_ SCALAR (Proxy @a)
  introspect :: GQLType a => Proxy a -> TypeLib -> TypeLib
  introspect _ = updateLib scalarTypeOf [] (Proxy @a)

instance GQLScalar Text where
  parseValue (String x) = pure x
  parseValue isType     = internalTypeMismatch "String" (Scalar isType)
  serialize = String

instance GQLScalar Bool where
  parseValue (Boolean x) = pure x
  parseValue isType      = internalTypeMismatch "Boolean" (Scalar isType)
  serialize = Boolean

instance GQLScalar Int where
  parseValue (Int x) = pure x
  parseValue isType  = internalTypeMismatch "Int" (Scalar isType)
  serialize = Int

instance GQLScalar Float where
  parseValue (Float x) = pure x
  parseValue isType    = internalTypeMismatch "Float" (Scalar isType)
  serialize = Float

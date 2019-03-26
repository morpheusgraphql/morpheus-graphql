{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Kind.Scalar where

import           Control.Monad                    ((>=>))
import           Data.Morpheus.Error.Internal     (internalTypeMismatch)
import           Data.Morpheus.Kind.GQLKind       (GQLKind (..), scalarTypeOf)
import           Data.Morpheus.Schema.Field       (createFieldWith)
import           Data.Morpheus.Schema.InputValue  (createInputValueWith)
import           Data.Morpheus.Schema.Utils.Utils (Field, InputValue, TypeLib)
import           Data.Morpheus.Types.Core         (Key)
import           Data.Morpheus.Types.Error        (Validation)
import qualified Data.Morpheus.Types.JSType       as Value (JSType (..), Scalar (..))
import           Data.Proxy                       (Proxy (..))

toScalar :: Value.JSType -> Validation Value.Scalar
toScalar (Value.Scalar x) = pure x
toScalar jsType           = internalTypeMismatch "Scalar" jsType

class Scalar a where
  parseValue :: Value.Scalar -> Validation a
  decode :: Value.JSType -> Validation a
  decode = toScalar >=> parseValue
  serialize :: a -> Value.Scalar
  encode :: a -> Value.JSType
  encode = Value.Scalar . serialize
  asInput :: Proxy a -> Key -> InputValue
  default asInput :: (Show a, GQLKind a) =>
    Proxy a -> Key -> InputValue
  asInput proxy name = createInputValueWith name (scalarTypeOf proxy)
  asField :: Proxy a -> Key -> Field
  default asField :: GQLKind a =>
    Proxy a -> Key -> Field
  asField proxy name = createFieldWith name (scalarTypeOf proxy) []
  introspect :: Proxy a -> TypeLib -> TypeLib
  default introspect :: GQLKind a =>
    Proxy a -> TypeLib -> TypeLib
  introspect = updateLib scalarTypeOf []

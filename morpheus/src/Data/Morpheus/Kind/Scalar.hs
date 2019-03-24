{-# LANGUAGE DefaultSignatures #-}

module Data.Morpheus.Kind.Scalar where

import           Data.Morpheus.Kind.GQLKind       (GQLKind (..), scalarTypeOf)
import           Data.Morpheus.Schema.InputValue  (createInputValueWith)
import           Data.Morpheus.Schema.Utils.Utils (Field, InputValue, TypeLib)
import           Data.Morpheus.Types.Core         (Key)
import           Data.Morpheus.Types.JSType       (JSType (..))
import           Data.Proxy                       (Proxy (..))

class Scalar a where
  parseValue :: JSType -> a
  serialize :: a -> JSType
  asInput :: Proxy a -> Key -> InputValue
  default asInput :: (Show a, GQLKind a) =>
    Proxy a -> Key -> InputValue
  asInput proxy name = createInputValueWith name (scalarTypeOf proxy)
  asField :: Proxy a -> Key -> Field
  introspect :: Proxy a -> TypeLib -> TypeLib
  default introspect :: GQLKind a =>
    Proxy a -> TypeLib -> TypeLib
  introspect = updateLib scalarTypeOf

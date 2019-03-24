{-# LANGUAGE DefaultSignatures #-}

module Data.Morpheus.Kind.Scalar where

import           Data.Morpheus.Kind.GQLKind       (GQLKind (..), scalarTypeOf)
import           Data.Morpheus.Schema.Utils.Utils (Field, TypeLib)
import           Data.Morpheus.Types.Core         (Key)
import           Data.Morpheus.Types.JSType       (JSType (..))
import           Data.Proxy                       (Proxy (..))

class Scalar a where
  parseValue :: JSType -> a
  serialize :: a -> JSType
  fieldType :: Proxy a -> Key -> Field
  introspect :: Proxy a -> TypeLib -> TypeLib
  default introspect :: GQLKind a =>
    Proxy a -> TypeLib -> TypeLib
  introspect = updateLib scalarTypeOf

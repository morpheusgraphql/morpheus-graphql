{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Morpheus.Kind.GQLKind
  ( GQLKind(..)
  , scalarTypeOf
  ) where

import           Data.Data                              (Typeable)
import           Data.Morpheus.Generics.Utils           (typeOf)
import           Data.Morpheus.Schema.Directive         (Directive)
import           Data.Morpheus.Schema.DirectiveLocation (DirectiveLocation)
import           Data.Morpheus.Schema.EnumValue         (EnumValue)
import           Data.Morpheus.Schema.Schema            (Schema)
import qualified Data.Morpheus.Schema.Type              as T (Type (..))
import           Data.Morpheus.Schema.TypeKind          (TypeKind (..))
import           Data.Morpheus.Schema.Utils.Utils       (Field, InputValue, Type)
import           Data.Morpheus.Types.Describer          (EnumOf (..), WithDeprecationArgs (..))
import           Data.Proxy                             (Proxy (..))
import           Data.Text                              (Text)

scalarTypeOf :: GQLKind a => Proxy a -> Type
scalarTypeOf proxy = buildType proxy SCALAR [] Nothing

class GQLKind a where
  description :: Proxy a -> Text
  default description :: Proxy a -> Text
  description _ = "default selection Description"
  typeID :: Proxy a -> Text
  default typeID :: Typeable a =>
    Proxy a -> Text
  typeID = typeOf
  buildType :: Proxy a -> TypeKind -> [Field] -> Maybe Type -> Type
  default buildType :: Proxy a -> TypeKind -> [Field] -> Maybe Type -> Type
  buildType proxy kind' fields' type' =
    T.Type
      { T.kind = EnumOf kind'
      , T.name = typeID proxy
      , T.description = description proxy
      , T.fields = WithDeprecationArgs fields'
      , T.ofType = type'
      , T.interfaces = []
      , T.possibleTypes = []
      , T.enumValues = WithDeprecationArgs []
      , T.inputFields = []
      }

instance GQLKind EnumValue where
  typeID _ = "__EnumValue"

instance GQLKind Type where
  typeID _ = "__Type"

instance GQLKind Field where
  typeID _ = "__Field"

instance GQLKind InputValue where
  typeID _ = "__InputValue"

instance GQLKind Schema where
  typeID _ = "__Schema"

instance GQLKind Directive where
  typeID _ = "__Directive"

instance GQLKind TypeKind where
  typeID _ = "__TypeKind"

instance GQLKind DirectiveLocation where
  typeID _ = "__DirectiveLocation"

instance GQLKind Int where
  typeID _ = "Int"

instance GQLKind Text where
  typeID _ = "String"

instance GQLKind Bool where
  typeID _ = "Boolean"

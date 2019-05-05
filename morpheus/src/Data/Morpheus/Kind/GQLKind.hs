{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Morpheus.Kind.GQLKind
  ( GQLKind(..)
  , scalarTypeOf
  , asObjectType
  , enumTypeOf
  , inputObjectOf
  , introspectScalar
  ) where

import           Data.Morpheus.Generics.TypeRep         (resolveTypes)
import           Data.Morpheus.Generics.Utils           (typeOf)
import           Data.Morpheus.Schema.Directive         (Directive)
import           Data.Morpheus.Schema.DirectiveLocation (DirectiveLocation)
import           Data.Morpheus.Schema.EnumValue         (EnumValue)
import           Data.Morpheus.Schema.Internal.Types    (Core (..), GObject (..), InputField, Leaf (..), LibType (..),
                                                         ObjectField (..), TypeLib, defineType, isTypeDefined)
import           Data.Morpheus.Schema.Schema            (Schema)
import           Data.Morpheus.Schema.TypeKind          (TypeKind (..))
import           Data.Morpheus.Schema.Utils.Utils       (Field, InputValue, Type)
import           Data.Morpheus.Types.Describer          ((::->))
import           Data.Proxy                             (Proxy (..))
import           Data.Text                              (Text)
import           Data.Typeable                          (Typeable)

scalarTypeOf :: GQLKind a => Proxy a -> LibType
scalarTypeOf = Leaf . LScalar . buildType

enumTypeOf :: GQLKind a => [Text] -> Proxy a -> LibType
enumTypeOf tags = Leaf . LEnum tags . buildType

asObjectType :: GQLKind a => [(Text, ObjectField)] -> Proxy a -> LibType
asObjectType fields = OutputObject . GObject fields . buildType

inputObjectOf :: GQLKind a => [(Text, InputField)] -> Proxy a -> LibType
inputObjectOf inputFields = InputObject . GObject inputFields . buildType

introspectScalar :: GQLKind a => Proxy a -> TypeLib -> TypeLib
introspectScalar = updateLib scalarTypeOf []

class GQLKind a where
  description :: Proxy a -> Text
  description _ = "default selection Description"
  typeID :: Proxy a -> Text
  default typeID :: Typeable a =>
    Proxy a -> Text
  typeID = typeOf
  buildType :: Proxy a -> Core
  buildType proxy = Core {name = typeID proxy, typeDescription = description proxy}
  updateLib :: (Proxy a -> LibType) -> [TypeLib -> TypeLib] -> Proxy a -> TypeLib -> TypeLib
  updateLib typeBuilder stack proxy lib' =
    if isTypeDefined (typeID proxy) lib'
      then lib'
      else resolveTypes lib' (defineType (typeID proxy, typeBuilder proxy) : stack)

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

instance GQLKind Float where
  typeID _ = "Int"

instance GQLKind Text where
  typeID _ = "String"

instance GQLKind Bool where
  typeID _ = "Boolean"

instance GQLKind a => GQLKind (Maybe a) where
  typeID _ = typeID (Proxy @a)

instance GQLKind a => GQLKind [a] where
  typeID _ = typeID (Proxy @a)

instance GQLKind a => GQLKind (p ::-> a) where
  typeID _ = typeID (Proxy @a)

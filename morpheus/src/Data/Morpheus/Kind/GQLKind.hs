{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Morpheus.Kind.GQLKind
  ( GQLKind(..)
  , scalarTypeOf
  , asObjectType
  , enumTypeOf
  , inputObjectOf
  ) where

import           Data.Data                              (Typeable)
import qualified Data.Map                               as M (insert, lookup)
import           Data.Morpheus.Generics.TypeRep         (resolveTypes)
import           Data.Morpheus.Generics.Utils           (typeOf)
import           Data.Morpheus.Schema.Directive         (Directive)
import           Data.Morpheus.Schema.DirectiveLocation (DirectiveLocation)
import           Data.Morpheus.Schema.EnumValue         (EnumValue)
import           Data.Morpheus.Schema.Internal.Types    (Core (..), GObject (..), GType (..), InputField,
                                                         InternalType (..), ObjectField (..), TypeLib)
import           Data.Morpheus.Schema.Schema            (Schema)
import           Data.Morpheus.Schema.TypeKind          (TypeKind (..))
import           Data.Morpheus.Schema.Utils.Utils       (Field, InputValue, Type)
import           Data.Proxy                             (Proxy (..))
import           Data.Text                              (Text)

scalarTypeOf :: GQLKind a => Proxy a -> GType
scalarTypeOf = OType . Scalar . buildType

enumTypeOf :: GQLKind a => [Text] -> Proxy a -> GType
enumTypeOf tags = OType . Enum tags . buildType

asObjectType :: GQLKind a => [(Text, ObjectField)] -> Proxy a -> GType
asObjectType fields = OType . Object . GObject fields . buildType

inputObjectOf :: GQLKind a => [(Text, InputField)] -> Proxy a -> GType
inputObjectOf inputFields = IType . Object . GObject inputFields . buildType

class GQLKind a where
  description :: Proxy a -> Text
  description _ = "default selection Description"
  typeID :: Proxy a -> Text
  default typeID :: Typeable a =>
    Proxy a -> Text
  typeID = typeOf
  buildType :: Proxy a -> Core
  buildType proxy = Core {name = typeID proxy, typeDescription = description proxy}
  updateLib :: (Proxy a -> GType) -> [TypeLib -> TypeLib] -> Proxy a -> TypeLib -> TypeLib
  updateLib typeBuilder stack proxy lib' =
    case M.lookup (typeID proxy) lib' of
      Just _ -> lib'
      Nothing -> resolveTypes lib' ([addType] ++ stack)
        where addType = M.insert (typeID proxy) (typeBuilder proxy)

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

instance GQLKind a => GQLKind (Maybe a) where
  typeID _ = typeID (Proxy @a)

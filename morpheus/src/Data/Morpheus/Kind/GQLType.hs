{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Morpheus.Kind.GQLType
  ( GQLType(..)
  , scalarTypeOf
  , asObjectType
  , enumTypeOf
  , inputObjectOf
  ) where

import           Data.Morpheus.Generics.ObjectRep       (resolveTypes)
import           Data.Morpheus.Generics.TypeID          (TypeID, typeId)
import           Data.Morpheus.Schema.Directive         (Directive)
import           Data.Morpheus.Schema.DirectiveLocation (DirectiveLocation)
import           Data.Morpheus.Schema.EnumValue         (EnumValue)
import           Data.Morpheus.Schema.Internal.AST    (Core (..), GObject (..), InputField, Leaf (..), LibType (..),
                                                         ObjectField (..), TypeLib, defineType, isTypeDefined)
import qualified Data.Morpheus.Schema.Internal.AST    as I (Field (..))
import           Data.Morpheus.Schema.Schema            (Schema)
import           Data.Morpheus.Schema.TypeKind          (TypeKind (..))
import           Data.Morpheus.Schema.Utils.Utils       (Field, InputValue, Type)
import           Data.Morpheus.Types.Describer          ((::->))
import           Data.Proxy                             (Proxy (..))
import           Data.Text                              (Text)
import           GHC.Generics

scalarTypeOf :: GQLType a => Proxy a -> LibType
scalarTypeOf = Leaf . LScalar . buildType

enumTypeOf :: GQLType a => [Text] -> Proxy a -> LibType
enumTypeOf tags = Leaf . LEnum tags . buildType

asObjectType :: GQLType a => [(Text, ObjectField)] -> Proxy a -> LibType
asObjectType fields = OutputObject . GObject fields . buildType

inputObjectOf :: GQLType a => [(Text, InputField)] -> Proxy a -> LibType
inputObjectOf inputFields = InputObject . GObject inputFields . buildType

class GQLType a where
  description :: Proxy a -> Text
  description _ = "default selection Description"
  typeID :: Proxy a -> Text
  default typeID :: (TypeID (Rep a), Generic a) =>
    Proxy a -> Text
  typeID = typeId
  field_ :: TypeKind -> Proxy a -> Text -> I.Field
  field_ kind' proxy' name' =
    I.Field {I.fieldName = name', I.notNull = True, I.asList = False, I.kind = kind', I.fieldType = typeID proxy'}
  buildType :: Proxy a -> Core
  buildType proxy = Core {name = typeID proxy, typeDescription = description proxy}
  updateLib :: (Proxy a -> LibType) -> [TypeLib -> TypeLib] -> Proxy a -> TypeLib -> TypeLib
  updateLib typeBuilder stack proxy lib' =
    if isTypeDefined (typeID proxy) lib'
      then lib'
      else resolveTypes lib' (defineType (typeID proxy, typeBuilder proxy) : stack)

instance GQLType EnumValue where
  typeID _ = "__EnumValue"

instance GQLType Type where
  typeID _ = "__Type"

instance GQLType Field where
  typeID _ = "__Field"

instance GQLType InputValue where
  typeID _ = "__InputValue"

instance GQLType Schema where
  typeID _ = "__Schema"

instance GQLType Directive where
  typeID _ = "__Directive"

instance GQLType TypeKind where
  typeID _ = "__TypeKind"

instance GQLType DirectiveLocation where
  typeID _ = "__DirectiveLocation"

instance GQLType Int where
  typeID _ = "Int"

instance GQLType Float where
  typeID _ = "Int"

instance GQLType Text where
  typeID _ = "String"

instance GQLType Bool where
  typeID _ = "Boolean"

instance GQLType a => GQLType (Maybe a) where
  typeID _ = typeID (Proxy @a)

instance GQLType a => GQLType [a] where
  typeID _ = typeID (Proxy @a)

instance GQLType a => GQLType (p ::-> a) where
  typeID _ = typeID (Proxy @a)

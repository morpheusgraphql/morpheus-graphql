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

import           Data.Morpheus.Generics.ObjectRep                  (resolveTypes)
import           Data.Morpheus.Generics.TypeID                     (TypeID, typeId)
import           Data.Morpheus.Schema.Directive                    (Directive)
import           Data.Morpheus.Schema.DirectiveLocation            (DirectiveLocation)
import           Data.Morpheus.Schema.EnumValue                    (EnumValue)
import           Data.Morpheus.Schema.Internal.RenderIntrospection (Field, InputValue, Type)
import           Data.Morpheus.Schema.Schema                       (Schema)
import           Data.Morpheus.Schema.TypeKind                     (TypeKind (..))
import           Data.Morpheus.Types.Internal.Data                 (DataField (..), DataFullType (..), DataInputField,
                                                                    DataLeaf (..), DataOutputField, DataScalarValidator,
                                                                    DataType (..), DataTypeLib, DataTypeWrapper (..),
                                                                    defineType, isTypeDefined)
import           Data.Morpheus.Types.Resolver                      ((::->))
import           Data.Proxy                                        (Proxy (..))
import           Data.Text                                         (Text)
import           GHC.Generics

scalarTypeOf :: GQLType a => DataScalarValidator -> Proxy a -> DataFullType
scalarTypeOf validator = Leaf . LeafScalar . buildType validator

enumTypeOf :: GQLType a => [Text] -> Proxy a -> DataFullType
enumTypeOf tags' = Leaf . LeafEnum . buildType tags'

asObjectType :: GQLType a => [(Text, DataOutputField)] -> Proxy a -> DataFullType
asObjectType fields' = OutputObject . buildType fields'

inputObjectOf :: GQLType a => [(Text, DataInputField)] -> Proxy a -> DataFullType
inputObjectOf fields' = InputObject . buildType fields'

class GQLType a where
  description :: Proxy a -> Text
  description _ = ""
  typeID :: Proxy a -> Text
  default typeID :: (TypeID (Rep a), Generic a) =>
    Proxy a -> Text
  typeID = typeId
  field_ :: TypeKind -> Proxy a -> t -> Text -> DataField t
  field_ kind' proxy' args' name' =
    DataField
      { fieldName = name'
      , fieldTypeWrappers = [NonNullType]
      , fieldKind = kind'
      , fieldType = typeID proxy'
      , fieldArgs = args'
      }
  buildType :: t -> Proxy a -> DataType t
  buildType typeData' proxy =
    DataType {typeName = typeID proxy, typeDescription = description proxy, typeData = typeData'}
  updateLib :: (Proxy a -> DataFullType) -> [DataTypeLib -> DataTypeLib] -> Proxy a -> DataTypeLib -> DataTypeLib
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

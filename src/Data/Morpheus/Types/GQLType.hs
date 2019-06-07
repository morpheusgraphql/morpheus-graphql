{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Morpheus.Types.GQLType
  ( GQLType(..)
  , scalarTypeOf
  , asObjectType
  , enumTypeOf
  , inputObjectOf
  ) where

import           Data.Morpheus.Resolve.Generics.ObjectRep          (resolveTypes)
import           Data.Morpheus.Resolve.Generics.TypeID             (TypeID, __typeId, __typeName)
import           Data.Morpheus.Schema.Directive                    (Directive)
import           Data.Morpheus.Schema.DirectiveLocation            (DirectiveLocation)
import           Data.Morpheus.Schema.EnumValue                    (EnumValue)
import           Data.Morpheus.Schema.Internal.RenderIntrospection (Field, InputValue, Type)
import           Data.Morpheus.Schema.Schema                       (Schema)
import           Data.Morpheus.Schema.TypeKind                     (TypeKind (..))
import           Data.Morpheus.Types.Internal.Data                 (DataField (..), DataFullType (..), DataInputField,
                                                                    DataLeaf (..), DataOutputField, DataType (..),
                                                                    DataTypeLib, DataTypeWrapper (..), DataValidator,
                                                                    defineType, isTypeDefined)
import           Data.Morpheus.Types.Internal.Validation           (SchemaValidation)
import           Data.Morpheus.Types.Resolver                      ((::->))
import           Data.Proxy                                        (Proxy (..))
import           Data.Text                                         (Text)
import           GHC.Generics

scalarTypeOf :: GQLType a => DataValidator -> Proxy a -> DataFullType
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
  _typeName :: Proxy a -> Text
  default _typeName :: (TypeID (Rep a), Generic a) =>
    Proxy a -> Text
  _typeName = __typeName
  _typeId :: Proxy a -> Text
  default _typeId :: (TypeID (Rep a), Generic a) =>
    Proxy a -> Text
  _typeId = __typeId
  field_ :: TypeKind -> Proxy a -> t -> Text -> DataField t
  field_ kind' proxy' args' name' =
    DataField
      { fieldName = name'
      , fieldTypeWrappers = [NonNullType]
      , fieldKind = kind'
      , fieldType = _typeName proxy'
      , fieldArgs = args'
      }
  buildType :: t -> Proxy a -> DataType t
  buildType typeData' proxy =
    DataType
      {typeName = _typeName proxy, typeHash = _typeId proxy, typeDescription = description proxy, typeData = typeData'}
  updateLib ::
       (Proxy a -> DataFullType)
    -> [DataTypeLib -> SchemaValidation DataTypeLib]
    -> Proxy a
    -> DataTypeLib
    -> SchemaValidation DataTypeLib
  updateLib typeBuilder stack proxy lib' =
    case isTypeDefined (_typeName proxy) lib' of
      Nothing -> resolveTypes (defineType (_typeName proxy, typeBuilder proxy) lib') stack
      Just hash'
        | hash' == _typeId proxy -> return lib'
      Just hash' -> Left $ "Name Conflict: " <> hash' <> " != " <> _typeId proxy

instance GQLType EnumValue where
  _typeName _ = "__EnumValue"

instance GQLType Type where
  _typeName _ = "__Type"

instance GQLType Field where
  _typeName _ = "__Field"

instance GQLType InputValue where
  _typeName _ = "__InputValue"

instance GQLType Schema where
  _typeName _ = "__Schema"

instance GQLType Directive where
  _typeName _ = "__Directive"

instance GQLType TypeKind where
  _typeName _ = "__TypeKind"

instance GQLType DirectiveLocation where
  _typeName _ = "__DirectiveLocation"

instance GQLType Int where
  _typeName _ = "Int"
  _typeId = const "__.INT"

instance GQLType Float where
  _typeName _ = "Float"
  _typeId _ = "__.FLOAT"

instance GQLType Text where
  _typeName _ = "String"
  _typeId _ = "__.STRING"

instance GQLType Bool where
  _typeName _ = "Boolean"
  _typeId _ = "__.BOOLEAN"

instance GQLType a => GQLType (Maybe a) where
  _typeName _ = _typeName (Proxy @a)
  _typeId _ = _typeId (Proxy @a)

instance GQLType a => GQLType [a] where
  _typeName _ = _typeName (Proxy @a)
  _typeId _ = _typeId (Proxy @a)

instance GQLType a => GQLType (p ::-> a) where
  _typeName _ = _typeName (Proxy @a)
  _typeId _ = _typeId (Proxy @a)

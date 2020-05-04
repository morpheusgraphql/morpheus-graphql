{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Morpheus.Schema.Schema
  ( defaultTypes,
  )
where

-- MORPHEUS
import Data.Morpheus.Schema.TypeKind (TypeKind)
import Data.Morpheus.Types.Internal.AST
  ( ArgumentsDefinition (..),
    FieldDefinition (..),
    FieldsDefinition,
    Name,
    TypeContent (..),
    TypeDefinition (..),
    TypeUpdater,
    TypeWrapper (..),
    createField,
    createScalarType,
    createType,
    insertType,
    unsafeFromFields,
  )
import Data.Morpheus.Types.Internal.Operation (Listable (..), Singleton (..))
import Data.Morpheus.Types.Internal.Resolving
  ( resolveUpdates,
  )
import Data.Text (Text)

type S__TypeKind = TypeKind

object :: Name -> [FieldDefinition] -> TypeDefinition
object name fields = createType name (DataObject [] (unsafeFromFields fields))

field :: Name -> Name -> FieldDefinition
field name typename = createField NoArguments name ([], typename)

field' :: Name -> Name -> FieldDefinition
field' name typename = createField NoArguments name ([TypeMaybe], typename)

list :: Name -> Name -> FieldDefinition
list name typename = createField NoArguments name ([TypeList], typename)

list' :: Name -> Name -> FieldDefinition
list' name typename = createField NoArguments name ([TypeMaybe, TypeList], typename)

depList :: Name -> Name -> FieldDefinition
depList name typename = createField (singleton undefined) name ([TypeMaybe, TypeList], typename)

schemaTypes = [__Schema, __Type, __Field]

__Schema :: TypeDefinition
__Schema =
  object
    "__Schema" -- type __Schema {
    [ list "types" "__Type", --   types: [__Type!]!
      field "queryType" "__Type", --   queryType: __Type!
      field' "mutationType" "__Type", --   mutationType: __Type
      field' "subscriptionType" "__Type", --   subscriptionType: __Type
      list "directives" "__Directive" --   directives: [__Directive!]!
    ]

__Type :: TypeDefinition
__Type =
  object
    "__Type" -- type __Type {
    [ field "kind" "__TypeKind", --   kind: __TypeKind!
      field' "name" "String", --   name: String
      field' "description" "String", --   description: String
      depList "fields" "__Field", --   fields(includeDeprecated: Boolean = false): [__Field!]
      list' "interfaces" "__Type", --   interfaces: [__Type!]
      list' "possibleTypes" "__Type", --   possibleTypes: [__Type!]
      depList "enumValues" "__Type", --   enumValues(includeDeprecated: Boolean = false): [__EnumValue!]
      list' "inputFields" "__InputValue", --   inputFields: [__InputValue!]
      field' "ofType" "__Type" --   ofType: __Type
    ]

__Field :: TypeDefinition
__Field =
  object
    "__Field" -- type __Field {
    [ field "name" "String", --   name: String!
      field' "description" "String", --   description: String
      list "args" "__InputValue", --   args: [__InputValue!]!
      field "type" "__Type", --   type: __Type!
      field "isDeprecated" "Boolean", --   isDeprecated: Boolean!
      field' "deprecationReason" "String" --   deprecationReason: String
    ]

__InputValue :: TypeDefinition
__InputValue =
  object
    "__InputValue" -- type __InputValue {
    [ field "name" "String", --   name: String!
      field' "description" "String", --   description: String
      field "type" "__Type", --   type: __Type!
      field' "defaultValue" "defaultValue" --   defaultValue: String
    ]

__EnumValue :: TypeDefinition
__EnumValue =
  object
    "__EnumValue" -- type __EnumValue {
    [ field "name" "String", --   name: String!
      field' "description" "String", --   description: String
      field "isDeprecated" "Boolean", --   isDeprecated: Boolean!
      field' "deprecationReason" "String" --   deprecationReason: String
    ]

__Directive :: TypeDefinition
__Directive =
  object
    "__Directive" -- type __Directive {
    [ field "name" "String", --   name: String!
      field' "description" "String", --   description: String
      list "locations" "__DirectiveLocation", --   locations: [__DirectiveLocation!]!
      list "args" "__InputValue" -- args: [__InputValue!]!
    ]

root :: TypeDefinition
root =
  object
    "__Root" -- type Root  {
    [ field' "__type" "String", --    __type(name: String!): __Type
      field "schema" "__Schema" --   __schema : __Schema!
    ]

-- enum __DirectiveLocation {
--   QUERY
--   MUTATION
--   SUBSCRIPTION
--   FIELD
--   FRAGMENT_DEFINITION
--   FRAGMENT_SPREAD
--   INLINE_FRAGMENT
--   SCHEMA
--   SCALAR
--   OBJECT
--   FIELD_DEFINITION
--   ARGUMENT_DEFINITION
--   INTERFACE
--   UNION
--   ENUM
--   ENUM_VALUE
--   INPUT_OBJECT
--   INPUT_FIELD_DEFINITION
-- }

defaultTypes :: TypeUpdater
defaultTypes =
  flip
    resolveUpdates
    $ fmap
      insertType
    $ [ createScalarType "Boolean",
        createScalarType "Int",
        createScalarType "Float",
        createScalarType "String",
        createScalarType "ID"
      ]
      <> schemaTypes

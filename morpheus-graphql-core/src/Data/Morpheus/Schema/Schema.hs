{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Morpheus.Schema.Schema
  ( withSystemTypes,
  )
where

-- MORPHEUS

import Data.Morpheus.Internal.Utils
  ( (<:>),
    Merge (..),
    singleton,
  )
import Data.Morpheus.QuasiQuoter (dsl)
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ArgumentsDefinition (..),
    DataFingerprint (..),
    FieldsDefinition,
    Message,
    Schema (..),
    TypeContent (..),
    TypeDefinition (..),
    TypeUpdater,
    TypeWrapper (..),
    createArgument,
    createField,
    insertType,
    internalFingerprint,
    unsafeFromFields,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( failure,
    resolveUpdates,
  )

withSystemTypes :: TypeUpdater
withSystemTypes s@Schema {query = q@TypeDefinition {typeContent = DataObject inter fields}} =
  ( do
      fs <- fields <:> hiddenFields
      pure $ s {query = q {typeContent = DataObject inter fs}}
  )
    >>= (`resolveUpdates` map (insertType . internalType) schemaTypes)
withSystemTypes _ = failure ("Query must be an Object Type" :: Message)

hiddenFields :: FieldsDefinition
hiddenFields =
  unsafeFromFields
    [ createField
        (singleton (createArgument "name" ([], "String")))
        "__type"
        ([TypeMaybe], "__Type"),
      createField
        NoArguments
        "__schema"
        ([], "__Schema")
    ]

internalType :: TypeDefinition a -> TypeDefinition a
internalType
  tyDef@TypeDefinition
    { typeFingerprint = DataFingerprint name xs
    } =
    tyDef {typeFingerprint = internalFingerprint name xs}

schemaTypes :: [TypeDefinition ANY]
schemaTypes =
  [dsl|

# default scalars
scalar Boolean
scalar Int
scalar Float
scalar String
scalar ID

type __Schema {
  types: [__Type!]!
  queryType: __Type!
  mutationType: __Type
  subscriptionType: __Type
  directives: [__Directive!]!
}

type __Type {
  kind: __TypeKind!
  name: String
  description: String

  # OBJECT and INTERFACE only
  fields(includeDeprecated: Boolean = false): [__Field!]

  # OBJECT only
  interfaces: [__Type!]

  # INTERFACE and UNION only
  possibleTypes: [__Type!]

  # ENUM only
  enumValues(includeDeprecated: Boolean = false): [__EnumValue!]

  # INPUT_OBJECT only
  inputFields: [__InputValue!]

  # NON_NULL and LIST only
  ofType: __Type
}

type __Field {
  name: String!
  description: String
  args: [__InputValue!]!
  type: __Type!
  isDeprecated: Boolean!
  deprecationReason: String
}

type __InputValue {
  name: String!
  description: String
  type: __Type!
  defaultValue: String
}

type __EnumValue {
  name: String!
  description: String
  isDeprecated: Boolean!
  deprecationReason: String
}

type __Directive {
  name: String!
  description: String
  locations: [__DirectiveLocation!]!
  args: [__InputValue!]!
}

enum __DirectiveLocation {
  QUERY
  MUTATION
  SUBSCRIPTION
  FIELD
  FRAGMENT_DEFINITION
  FRAGMENT_SPREAD
  INLINE_FRAGMENT
  SCHEMA
  SCALAR
  OBJECT
  FIELD_DEFINITION
  ARGUMENT_DEFINITION
  INTERFACE
  UNION
  ENUM
  ENUM_VALUE
  INPUT_OBJECT
  INPUT_FIELD_DEFINITION
}

enum __TypeKind {
  SCALAR
  OBJECT
  INTERFACE
  UNION
  ENUM
  INPUT_OBJECT
  LIST
  NON_NULL
}

|]

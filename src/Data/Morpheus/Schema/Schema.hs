{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Data.Morpheus.Schema.Schema
  ( initSchema
  , findType
  , S__Schema(..)
  , S__Type(..)
  , S__Field(..)
  , S__EnumValue(..)
  , S__InputValue(..)
  ) where

import           Data.Text                                (Text)

-- MORPHEUS
import           Data.Morpheus.Execution.Document.Compile (gqlDocumentNamespace)
import           Data.Morpheus.Schema.TypeKind
import           Data.Morpheus.Types.Internal.Data        (DataObject, DataTypeLib (..), allDataTypes)

type S__TypeKind = TypeKind

[gqlDocumentNamespace|
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
  fields(includeDeprecated: Boolean ): [__Field!]

  # OBJECT only
  interfaces: [__Type!]

  # INTERFACE and UNION only
  possibleTypes: [__Type!]

  # ENUM only
  enumValues(includeDeprecated: Boolean): [__EnumValue!]

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

|]

convertTypes :: DataTypeLib -> Either String [S__Type m]
convertTypes lib = traverse (`renderType` lib) (allDataTypes lib)

buildSchemaLinkType :: (Text, DataObject) -> S__Type m
buildSchemaLinkType (key', _) = createObjectType key' Nothing $ Just []

findType :: Text -> DataTypeLib -> Maybe (S__Type m)
findType name lib = (name, ) <$> lookup name (allDataTypes lib) >>= renderT
  where
    renderT i =
      case renderType i lib of
        Left _  -> Nothing
        Right x -> Just x

initSchema :: DataTypeLib -> Either String (S__Schema (Either String))
initSchema lib =
  pure
    S__Schema
      { s__SchemaTypes = const $ convertTypes lib
      , s__SchemaQueryType = const $ pure $ buildSchemaLinkType $ query lib
      , s__SchemaMutationType = const $ pure $ buildSchemaLinkType <$> mutation lib
      , s__SchemaSubscriptionType = const $ pure $ buildSchemaLinkType <$> subscription lib
      , s__SchemaDirectives = const $ return []
      }

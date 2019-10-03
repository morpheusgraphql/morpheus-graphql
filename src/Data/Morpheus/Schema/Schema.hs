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
  , Schema
  , Type
  ) where

import           Data.Text                                (Text)

-- MORPHEUS
import           Data.Morpheus.Execution.Document.Compile (gqlDocumentNamespace)
import           Data.Morpheus.Schema.DirectiveLocation
import           Data.Morpheus.Types.Internal.Data        (DataObject, DataTypeLib (..), allDataTypes)

type Boolean = Bool

[gqlDocumentNamespace|

  type Schema {
    types: [Type!]!
    queryType: Type!
    mutationType: Type
    subscriptionType: Type
    directives: [Directive!]!
  }

  type Type {
    kind: TypeKind!
    name: String
    description: String

    # OBJECT and INTERFACE only
    fields(includeDeprecated: Boolean ): [Field!]

    # OBJECT only
    interfaces: [Type!]

    # INTERFACE and UNION only
    possibleTypes: [Type!]

    # ENUM only
    enumValues(includeDeprecated: Boolean ): [EnumValue!]

    # INPUT_OBJECT only
    inputFields: [InputValue!]

    # NON_NULL and LIST only
    ofType: Type
  }

  type Field {
    name: String!
    description: String
    args: [InputValue!]!
    type: Type!
    isDeprecated: Boolean!
    deprecationReason: String
  }

  type InputValue {
    name: String!
    description: String
    type: Type!
    defaultValue: String
  }

  type EnumValue {
    name: String!
    description: String
    isDeprecated: Boolean!
    deprecationReason: String
  }

  enum TypeKind {
    SCALAR
    OBJECT
    INTERFACE
    UNION
    ENUM
    INPUT_OBJECT
    LIST
    NON_NULL
  }

  type Directive {
    name: String!
    description: String
    locations: [DirectiveLocation!]!
    args: [InputValue!]!
  }

|]

convertTypes :: DataTypeLib -> Either String [Type m]
convertTypes lib = traverse (`renderType` lib) (allDataTypes lib)

buildSchemaLinkType :: (Text, DataObject) -> Type m
buildSchemaLinkType (key', _) = createObjectType key' Nothing $ Just []

findType :: Text -> DataTypeLib -> Maybe (Type m)
findType name lib = (name, ) <$> lookup name (allDataTypes lib) >>= renderT
  where
    renderT i =
      case renderType i lib of
        Left _  -> Nothing
        Right x -> Just x

initSchema :: DataTypeLib -> Either String (Schema (Either String))
initSchema lib =
  pure
    Schema
      { schemaTypes = const $ convertTypes lib
      , schemaQueryType = const $ pure $ buildSchemaLinkType $ query lib
      , schemaMutationType = const $ pure $ buildSchemaLinkType <$> mutation lib
      , schemaSubscriptionType = const $ pure $ buildSchemaLinkType <$> subscription lib
      , schemaDirectives = const $ return []
      }

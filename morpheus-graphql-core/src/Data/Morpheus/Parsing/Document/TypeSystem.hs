{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Parsing.Document.TypeSystem
  ( parseSchema,
    parseTypeDefinitions,
  )
where

-- MORPHEUS

import Control.Applicative ((*>), Applicative (..))
import Control.Monad ((>=>))
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (foldr)
import Data.Functor ((<$>), fmap)
import Data.Maybe (Maybe (..))
import Data.Morpheus.Error.NameCollision (NameCollision (..))
import Data.Morpheus.Parsing.Internal.Internal
  ( Parser,
    processParser,
  )
import Data.Morpheus.Parsing.Internal.Pattern
  ( argumentsDefinition,
    enumValueDefinition,
    fieldsDefinition,
    inputFieldsDefinition,
    optionalDirectives,
    parseDirectiveLocation,
    parseOperationType,
    typeDeclaration,
  )
import Data.Morpheus.Parsing.Internal.Terms
  ( at,
    collection,
    colon,
    equal,
    ignoredTokens,
    keyword,
    optDescription,
    optionalCollection,
    parseName,
    parseTypeName,
    pipe,
    sepByAnd,
    setOf,
  )
import Data.Morpheus.Parsing.Internal.Value
  ( Parse (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    CONST,
    DataFingerprint (..),
    Description,
    DirectiveDefinition (..),
    Directives,
    ELEM,
    FieldsDefinition,
    IN,
    LEAF,
    OBJECT,
    OUT,
    RawTypeDefinition (..),
    RootOperationTypeDefinition (..),
    ScalarDefinition (..),
    Schema,
    SchemaDefinition (..),
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeName,
    Value,
    buildSchema,
    mkUnionMember,
    toAny,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
    failure,
  )
import Text.Megaparsec
  ( (<|>),
    eof,
    label,
    manyTill,
    optional,
  )
import Prelude
  ( ($),
    (.),
  )

mkObject ::
  (ELEM OBJECT a ~ TRUE) =>
  Maybe Description ->
  TypeName ->
  [TypeName] ->
  Directives s ->
  FieldsDefinition OUT s ->
  TypeDefinition a s
mkObject typeDescription typeName objectImplements typeDirectives objectFields =
  TypeDefinition
    { typeFingerprint = DataFingerprint typeName [],
      typeContent = DataObject {objectImplements, objectFields},
      ..
    }

mkType ::
  Maybe Description ->
  TypeName ->
  Directives s ->
  TypeContent TRUE a s ->
  TypeDefinition a s
mkType typeDescription typeName typeDirectives typeContent =
  TypeDefinition
    { typeFingerprint = DataFingerprint typeName [],
      ..
    }

mkScalar ::
  ELEM LEAF a ~ TRUE =>
  Maybe Description ->
  TypeName ->
  Directives s ->
  TypeDefinition a s
mkScalar typeDescription typeName typeDirectives =
  TypeDefinition
    { typeFingerprint = DataFingerprint typeName [],
      typeContent = DataScalar (ScalarDefinition pure),
      ..
    }

-- Scalars : https://graphql.github.io/graphql-spec/June2018/#sec-Scalars
--
--  ScalarTypeDefinition:
--    Description(opt) scalar Name Directives(Const)(opt)
--
scalarTypeDefinition ::
  Parse (Value s) =>
  Maybe Description ->
  Parser (TypeDefinition ANY s)
scalarTypeDefinition typeDescription =
  label "ScalarTypeDefinition" $
    mkScalar typeDescription
      <$> typeDeclaration "scalar"
      <*> optionalDirectives

-- Objects : https://graphql.github.io/graphql-spec/June2018/#sec-Objects
--
--  ObjectTypeDefinition:
--    Description(opt) type Name ImplementsInterfaces(opt) Directives(Const)(opt) FieldsDefinition(opt)
--
--  ImplementsInterfaces
--    implements &(opt) NamedType
--    ImplementsInterfaces & NamedType
--
--  FieldsDefinition
--    { FieldDefinition(list) }
--
--  FieldDefinition
--    Description(opt) Name ArgumentsDefinition(opt) : Type Directives(Const)(opt)
--
objectTypeDefinition ::
  Parse (Value s) =>
  Maybe Description ->
  Parser (TypeDefinition OUT s)
objectTypeDefinition typeDescription =
  label "ObjectTypeDefinition" $
    mkObject typeDescription
      <$> typeDeclaration "type"
      <*> optionalImplementsInterfaces
      <*> optionalDirectives
      <*> fieldsDefinition

-- build object

optionalImplementsInterfaces :: Parser [TypeName]
optionalImplementsInterfaces = implements <|> pure []
  where
    implements =
      label "ImplementsInterfaces" $ keyword "implements" *> sepByAnd parseTypeName

-- Interfaces: https://graphql.github.io/graphql-spec/June2018/#sec-Interfaces
--
--  InterfaceTypeDefinition
--    Description(opt) interface Name Directives(Const)(opt) FieldsDefinition(opt)
--
interfaceTypeDefinition ::
  Parse (Value s) =>
  Maybe Description ->
  Parser (TypeDefinition OUT s)
interfaceTypeDefinition typeDescription =
  label "InterfaceTypeDefinition" $
    mkType typeDescription
      <$> typeDeclaration "interface"
      <*> optionalDirectives
      <*> (DataInterface <$> fieldsDefinition)

-- Unions : https://graphql.github.io/graphql-spec/June2018/#sec-Unions
--
--  UnionTypeDefinition:
--    Description(opt) union Name Directives(Const)(opt) UnionMemberTypes(opt)
--
--  UnionMemberTypes:
--    = |(opt) NamedType
--      UnionMemberTypes | NamedType
--
unionTypeDefinition ::
  Parse (Value s) =>
  Maybe Description ->
  Parser (TypeDefinition OUT s)
unionTypeDefinition typeDescription =
  label "UnionTypeDefinition" $
    mkType typeDescription
      <$> typeDeclaration "union"
      <*> optionalDirectives
      <*> (DataUnion <$> unionMemberTypes)
  where
    unionMemberTypes =
      equal
        *> pipe (mkUnionMember <$> parseTypeName)

-- Enums : https://graphql.github.io/graphql-spec/June2018/#sec-Enums
--
--  EnumTypeDefinition
--    Description(opt) enum Name Directives(Const)(opt) EnumValuesDefinition(opt)
--
--  EnumValuesDefinition
--    { EnumValueDefinition(list) }
--
--  EnumValueDefinition
--    Description(opt) EnumValue Directives(Const)(opt)
--
enumTypeDefinition ::
  Parse (Value s) =>
  Maybe Description ->
  Parser (TypeDefinition ANY s)
enumTypeDefinition typeDescription =
  label "EnumTypeDefinition" $
    mkType typeDescription
      <$> typeDeclaration "enum"
      <*> optionalDirectives
      <*> (DataEnum <$> collection enumValueDefinition)

-- Input Objects : https://graphql.github.io/graphql-spec/June2018/#sec-Input-Objects
--
--   InputObjectTypeDefinition
--     Description(opt) input Name  Directives(Const)(opt) InputFieldsDefinition(opt)
--
--   InputFieldsDefinition:
--     { InputValueDefinition(list) }
--
inputObjectTypeDefinition ::
  Parse (Value s) =>
  Maybe Description ->
  Parser (TypeDefinition IN s)
inputObjectTypeDefinition typeDescription =
  label "InputObjectTypeDefinition" $
    mkType
      typeDescription
      <$> typeDeclaration "input"
      <*> optionalDirectives
      <*> (DataInputObject <$> inputFieldsDefinition)

-- 3.13 DirectiveDefinition
--
--  DirectiveDefinition:
--     Description[opt] directive @ Name ArgumentsDefinition[opt] repeatable[opt] on DirectiveLocations
--
--  DirectiveLocations:
--    DirectiveLocations | DirectiveLocation
--    |[opt] DirectiveLocation
parseDirectiveDefinition ::
  Parse (Value s) =>
  Maybe Description ->
  Parser (DirectiveDefinition s)
parseDirectiveDefinition directiveDefinitionDescription =
  label "DirectiveDefinition" $
    DirectiveDefinition
      <$> ( keyword "directive"
              *> at
              *> parseName
          )
        <*> pure directiveDefinitionDescription
        <*> optionalCollection argumentsDefinition
        <*> (optional (keyword "repeatable") *> keyword "on" *> pipe parseDirectiveLocation)

-- 3.2 Schema
-- SchemaDefinition:
--    schema Directives[Const,opt]
--      { RootOperationTypeDefinition(list) }
--
--  RootOperationTypeDefinition:
--    OperationType: NamedType

-- data SchemaDefinition = SchemaDefinition
--   { query :: TypeName,
--     mutation :: Maybe TypeName,
--     subscription :: Maybe TypeName
--   }
parseSchemaDefinition :: Maybe Description -> Parser SchemaDefinition
parseSchemaDefinition _schemaDescription =
  label "SchemaDefinition" $
    keyword "schema"
      *> ( SchemaDefinition
             <$> optionalDirectives
             <*> setOf parseRootOperationTypeDefinition
         )

parseRootOperationTypeDefinition :: Parser RootOperationTypeDefinition
parseRootOperationTypeDefinition =
  RootOperationTypeDefinition
    <$> (parseOperationType <* colon)
    <*> parseTypeName

parseTypeSystemUnit ::
  Parser RawTypeDefinition
parseTypeSystemUnit =
  label "TypeDefinition" $
    do
      description <- optDescription
      -- scalar | enum |  input | object | union | interface
      types description
        <|> RawSchemaDefinition <$> parseSchemaDefinition description
        <|> RawDirectiveDefinition <$> parseDirectiveDefinition description
  where
    types description =
      RawTypeDefinition
        <$> ( (toAny <$> inputObjectTypeDefinition description)
                <|> (toAny <$> unionTypeDefinition description)
                <|> enumTypeDefinition description
                <|> scalarTypeDefinition description
                <|> (toAny <$> objectTypeDefinition description)
                <|> (toAny <$> interfaceTypeDefinition description)
            )

typePartition ::
  [RawTypeDefinition] ->
  ( [SchemaDefinition],
    [TypeDefinition ANY CONST],
    [DirectiveDefinition CONST]
  )
typePartition = foldr split ([], [], [])

split ::
  RawTypeDefinition ->
  ( [SchemaDefinition],
    [TypeDefinition ANY CONST],
    [DirectiveDefinition CONST]
  ) ->
  ( [SchemaDefinition],
    [TypeDefinition ANY CONST],
    [DirectiveDefinition CONST]
  )
split (RawSchemaDefinition schema) (schemas, types, dirs) = (schema : schemas, types, dirs)
split (RawTypeDefinition ty) (schemas, types, dirs) = (schemas, ty : types, dirs)
split (RawDirectiveDefinition dir) (schemas, types, dirs) = (schemas, types, dir : dirs)

--  split (RawDirectiveDefinition d)

withSchemaDefinition ::
  ( [SchemaDefinition],
    [TypeDefinition ANY s],
    [DirectiveDefinition CONST]
  ) ->
  Eventless
    (Maybe SchemaDefinition, [TypeDefinition ANY s], [DirectiveDefinition CONST])
withSchemaDefinition ([], t, dirs) = pure (Nothing, t, dirs)
withSchemaDefinition ([x], t, dirs) = pure (Just x, t, dirs)
withSchemaDefinition (_ : xs, _, _) = failure (fmap nameCollision xs)

parseTypeSystemDefinition :: Parser [RawTypeDefinition]
parseTypeSystemDefinition =
  label "TypeSystemDefinitions" $
    ignoredTokens
      *> manyTill parseTypeSystemUnit eof

typeSystemDefinition ::
  ByteString ->
  Eventless
    ( Maybe SchemaDefinition,
      [TypeDefinition ANY CONST],
      [DirectiveDefinition CONST]
    )
typeSystemDefinition =
  processParser parseTypeSystemDefinition
    >=> withSchemaDefinition . typePartition

parseTypeDefinitions :: ByteString -> Eventless [TypeDefinition ANY CONST]
parseTypeDefinitions = fmap snd3 . typeSystemDefinition

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

parseSchema ::
  ByteString ->
  Eventless (Schema CONST)
parseSchema =
  typeSystemDefinition
    >=> buildSchema

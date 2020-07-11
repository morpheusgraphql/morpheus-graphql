{-# LANGUAGE FlexibleContexts #-}
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

import Control.Applicative ((*>), pure)
import Control.Monad ((>=>))
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
  ( collection,
    ignoredTokens,
    keyword,
    optDescription,
    optionalCollection,
    parseName,
    parseTypeName,
    pipe,
    sepByAnd,
    setOf,
    symbol,
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
    IN,
    OUT,
    RawTypeDefinition (..),
    RootOperationTypeDefinition (..),
    ScalarDefinition (..),
    SchemaDefinition (..),
    TypeContent (..),
    TypeDefinition (..),
    TypeName,
    Value,
    mkUnionMember,
    toAny,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
    failure,
  )
import Data.Text (Text)
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
    snd,
  )

-- Scalars : https://graphql.github.io/graphql-spec/June2018/#sec-Scalars
--
--  ScalarTypeDefinition:
--    Description(opt) scalar Name Directives(Const)(opt)
--
scalarTypeDefinition ::
  Parse (Value s) =>
  Maybe Description ->
  Parser (TypeDefinition ANY s)
scalarTypeDefinition typeDescription = label "ScalarTypeDefinition" $ do
  typeName <- typeDeclaration "scalar"
  typeDirectives <- optionalDirectives
  pure
    TypeDefinition
      { typeFingerprint = DataFingerprint typeName [],
        typeContent = DataScalar $ ScalarDefinition pure,
        ..
      }

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
objectTypeDefinition typeDescription = label "ObjectTypeDefinition" $ do
  typeName <- typeDeclaration "type"
  objectImplements <- optionalImplementsInterfaces
  typeDirectives <- optionalDirectives
  objectFields <- fieldsDefinition
  -- build object
  pure
    TypeDefinition
      { typeFingerprint = DataFingerprint typeName [],
        typeContent = DataObject {objectImplements, objectFields},
        ..
      }

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
interfaceTypeDefinition typeDescription = label "InterfaceTypeDefinition" $ do
  typeName <- typeDeclaration "interface"
  typeDirectives <- optionalDirectives
  typeContent <- DataInterface <$> fieldsDefinition
  pure
    TypeDefinition
      { typeFingerprint = DataFingerprint typeName [],
        ..
      }

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
unionTypeDefinition typeDescription = label "UnionTypeDefinition" $ do
  typeName <- typeDeclaration "union"
  typeDirectives <- optionalDirectives
  typeContent <- DataUnion <$> unionMemberTypes
  pure
    TypeDefinition
      { typeFingerprint = DataFingerprint typeName [],
        ..
      }
  where
    unionMemberTypes =
      symbol '='
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
enumTypeDefinition typeDescription = label "EnumTypeDefinition" $ do
  typeName <- typeDeclaration "enum"
  typeDirectives <- optionalDirectives
  typeContent <- DataEnum <$> collection enumValueDefinition
  pure
    TypeDefinition
      { typeFingerprint = DataFingerprint typeName [],
        ..
      }

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
  label "InputObjectTypeDefinition" $ do
    typeName <- typeDeclaration "input"
    typeDirectives <- optionalDirectives
    typeContent <- DataInputObject <$> inputFieldsDefinition
    -- build input
    pure
      TypeDefinition
        { typeFingerprint = DataFingerprint typeName [],
          ..
        }

-- 3.13 DirectiveDefinition
--
--  DirectiveDefinition:
--     Description[opt] directive @ Name ArgumentsDefinition[opt] repeatable[opt] on DirectiveLocations
--
--  DirectiveLocations:
--    DirectiveLocations | DirectiveLocation
--    |[opt] DirectiveLocation

parseDirectiveDefinition ::
  Parser RawTypeDefinition
parseDirectiveDefinition = label "DirectiveDefinition" $ do
  directiveDefinitionDescription <- optDescription
  keyword "directive"
  symbol '@'
  directiveDefinitionName <- parseName
  directiveDefinitionArgs <- optionalCollection argumentsDefinition
  _ <- optional (keyword "repeatable")
  keyword "on"
  directiveDefinitionLocations <- pipe parseDirectiveLocation
  pure
    $ RawDirectiveDefinition
    $ DirectiveDefinition
      { directiveDefinitionName,
        directiveDefinitionDescription,
        directiveDefinitionLocations,
        directiveDefinitionArgs
      }

-- 3.2 Schema
-- SchemaDefinition:
--    schema Directives[Const,opt]
--      { RootOperationTypeDefinitionlist }
--
--  RootOperationTypeDefinition:
--    OperationType: NamedType

-- data SchemaDefinition = SchemaDefinition
--   { query :: TypeName,
--     mutation :: Maybe TypeName,
--     subscription :: Maybe TypeName
--   }

parseSchemaDefinition :: Parser RawTypeDefinition
parseSchemaDefinition = label "SchemaDefinition" $ do
  keyword "schema"
  schemaDirectives <- optionalDirectives
  unSchemaDefinition <- setOf parseRootOperationTypeDefinition
  pure
    $ RawSchemaDefinition
    $ SchemaDefinition {schemaDirectives, unSchemaDefinition}

parseRootOperationTypeDefinition :: Parser RootOperationTypeDefinition
parseRootOperationTypeDefinition = do
  operationType <- parseOperationType
  symbol ':'
  RootOperationTypeDefinition operationType <$> parseTypeName

parseDataType ::
  Parse (Value s) =>
  Parser (TypeDefinition ANY s)
parseDataType = label "TypeDefinition" $ do
  description <- optDescription
  -- scalar | enum |  input | object | union | interface
  (toAny <$> inputObjectTypeDefinition description)
    <|> (toAny <$> unionTypeDefinition description)
    <|> enumTypeDefinition description
    <|> scalarTypeDefinition description
    <|> (toAny <$> objectTypeDefinition description)
    <|> (toAny <$> interfaceTypeDefinition description)

parseRawTypeDefinition ::
  Parser RawTypeDefinition
parseRawTypeDefinition =
  label "TypeSystemDefinitions" $
    RawTypeDefinition <$> parseDataType
      <|> parseSchemaDefinition
      <|> parseDirectiveDefinition

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
withSchemaDefinition (_ : xs, _, dirs) = failure (fmap (nameCollision "schema") xs, dirs)

parseTypeSystemDefinition :: Parser [RawTypeDefinition]
parseTypeSystemDefinition = label "TypeSystemDefinitions" $ do
  ignoredTokens
  manyTill parseRawTypeDefinition eof

parseTypeDefinitions :: Text -> Eventless [TypeDefinition ANY CONST]
parseTypeDefinitions = fmap snd . parseSchema

parseSchema :: Text -> Eventless (Maybe SchemaDefinition, [TypeDefinition ANY CONST])
parseSchema =
  processParser parseTypeSystemDefinition
    >=> withSchemaDefinition . typePartition

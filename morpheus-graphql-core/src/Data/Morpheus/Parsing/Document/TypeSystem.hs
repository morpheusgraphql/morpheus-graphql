{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Morpheus.Parsing.Document.TypeSystem
  ( parseSchema,
  )
where

-- MORPHEUS
import Data.Morpheus.Parsing.Internal.Internal
  ( Parser,
    processParser,
  )
import Data.Morpheus.Parsing.Internal.Pattern
  ( enumValueDefinition,
    fieldsDefinition,
    inputFieldsDefinition,
    optionalDirectives,
    parseOperationType,
    typeDeclaration,
  )
import Data.Morpheus.Parsing.Internal.Terms
  ( collection,
    keyword,
    litAssignment,
    operator,
    optDescription,
    parseTypeName,
    pipeLiteral,
    sepByAnd,
    setOf,
    spaceAndComments,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    DataFingerprint (..),
    Description,
    IN,
    OUT,
    OperationType,
    ScalarDefinition (..),
    SchemaDefinitionRaw (..),
    TypeContent (..),
    TypeDefinition (..),
    TypeName,
    mkUnionMember,
    toAny,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
  )
import Data.Text (Text)
import Text.Megaparsec
  ( (<|>),
    eof,
    label,
    manyTill,
    sepBy1,
  )

-- Scalars : https://graphql.github.io/graphql-spec/June2018/#sec-Scalars
--
--  ScalarTypeDefinition:
--    Description(opt) scalar Name Directives(Const)(opt)
--
scalarTypeDefinition :: Maybe Description -> Parser (TypeDefinition ANY)
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
objectTypeDefinition :: Maybe Description -> Parser (TypeDefinition OUT)
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
interfaceTypeDefinition :: Maybe Description -> Parser (TypeDefinition OUT)
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
unionTypeDefinition :: Maybe Description -> Parser (TypeDefinition OUT)
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
    unionMemberTypes = operator '=' *> (mkUnionMember <$> parseTypeName) `sepBy1` pipeLiteral

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
enumTypeDefinition :: Maybe Description -> Parser (TypeDefinition ANY)
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
inputObjectTypeDefinition :: Maybe Description -> Parser (TypeDefinition IN)
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

parseSchemaDefinition :: Parser SchemaDefinitionRaw
parseSchemaDefinition = label "SchemaDefinition" $ do
  keyword "schema"
  schemaDirectives <- optionalDirectives
  unSchemaDefinition <- setOf parseRootOperationTypeDefinition
  pure SchemaDefinitionRaw {schemaDirectives, unSchemaDefinition}

parseRootOperationTypeDefinition :: Parser (OperationType, TypeName)
parseRootOperationTypeDefinition = do
  operationType <- parseOperationType
  litAssignment -- ':'
  operationName <- parseTypeName
  pure (operationType, operationName)

parseDataType :: Parser (TypeDefinition ANY)
parseDataType = label "TypeDefinition" $ do
  description <- optDescription
  -- scalar | enum |  input | object | union | interface
  (toAny <$> inputObjectTypeDefinition description)
    <|> (toAny <$> unionTypeDefinition description)
    <|> enumTypeDefinition description
    <|> scalarTypeDefinition description
    <|> (toAny <$> objectTypeDefinition description)
    <|> (toAny <$> interfaceTypeDefinition description)

parseSchema :: Text -> Eventless [TypeDefinition ANY]
parseSchema = processParser request
  where
    request = label "DocumentTypes" $ do
      spaceAndComments
      manyTill parseDataType eof

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

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
    typDeclaration,
  )
import Data.Morpheus.Parsing.Internal.Terms
  ( collection,
    keyword,
    operator,
    optDescription,
    parseName,
    pipeLiteral,
    sepByAnd,
    spaceAndComments,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    DataFingerprint (..),
    Description,
    IN,
    Meta (..),
    Name,
    OUT,
    ScalarDefinition (..),
    TypeContent (..),
    TypeDefinition (..),
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
scalarTypeDefinition metaDescription = label "ScalarTypeDefinition" $ do
  typeName <- typDeclaration "scalar"
  metaDirectives <- optionalDirectives
  pure
    TypeDefinition
      { typeName,
        typeMeta = Just Meta {metaDescription, metaDirectives},
        typeFingerprint = DataFingerprint typeName [],
        typeContent = DataScalar $ ScalarDefinition pure
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
objectTypeDefinition metaDescription = label "ObjectTypeDefinition" $ do
  typeName <- typDeclaration "type"
  objectImplements <- optionalImplementsInterfaces
  metaDirectives <- optionalDirectives
  objectFields <- fieldsDefinition
  -- build object
  pure
    TypeDefinition
      { typeName,
        typeMeta = Just Meta {metaDescription, metaDirectives},
        typeFingerprint = DataFingerprint typeName [],
        typeContent = DataObject {objectImplements, objectFields}
      }

optionalImplementsInterfaces :: Parser [Name]
optionalImplementsInterfaces = implements <|> pure []
  where
    implements =
      label "ImplementsInterfaces" $ keyword "implements" *> sepByAnd parseName

-- Interfaces: https://graphql.github.io/graphql-spec/June2018/#sec-Interfaces
--
--  InterfaceTypeDefinition
--    Description(opt) interface Name Directives(Const)(opt) FieldsDefinition(opt)
--
interfaceTypeDefinition :: Maybe Description -> Parser (TypeDefinition OUT)
interfaceTypeDefinition metaDescription = label "InterfaceTypeDefinition" $ do
  typeName <- typDeclaration "interface"
  metaDirectives <- optionalDirectives
  fields <- fieldsDefinition
  -- build interface
  pure
    TypeDefinition
      { typeName,
        typeMeta = Just Meta {metaDescription, metaDirectives},
        typeFingerprint = DataFingerprint typeName [],
        typeContent = DataInterface fields
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
unionTypeDefinition metaDescription = label "UnionTypeDefinition" $ do
  typeName <- typDeclaration "union"
  metaDirectives <- optionalDirectives
  memberTypes <- unionMemberTypes
  -- build union
  pure
    TypeDefinition
      { typeName,
        typeMeta = Just Meta {metaDescription, metaDirectives},
        typeFingerprint = DataFingerprint typeName [],
        typeContent = DataUnion memberTypes
      }
  where
    unionMemberTypes = operator '=' *> parseName `sepBy1` pipeLiteral

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
enumTypeDefinition metaDescription = label "EnumTypeDefinition" $ do
  typeName <- typDeclaration "enum"
  metaDirectives <- optionalDirectives
  enumValuesDefinitions <- collection enumValueDefinition
  -- build enum
  pure
    TypeDefinition
      { typeName,
        typeContent = DataEnum enumValuesDefinitions,
        typeFingerprint = DataFingerprint typeName [],
        typeMeta = Just Meta {metaDescription, metaDirectives}
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
inputObjectTypeDefinition metaDescription =
  label "InputObjectTypeDefinition" $ do
    typeName <- typDeclaration "input"
    metaDirectives <- optionalDirectives
    fields <- inputFieldsDefinition
    -- build input
    pure
      TypeDefinition
        { typeName,
          typeContent = DataInputObject fields,
          typeFingerprint = DataFingerprint typeName [],
          typeMeta = Just Meta {metaDescription, metaDirectives}
        }

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

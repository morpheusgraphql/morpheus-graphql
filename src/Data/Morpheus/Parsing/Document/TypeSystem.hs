{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parsing.Document.TypeSystem
  ( parseDataType
  ) where

import           Data.Text                               (Text)
import           Text.Megaparsec                         (label, sepBy1, (<|>))

-- MORPHEUS
import           Data.Morpheus.Parsing.Internal.Create   (createEnumType, createScalarType, createType, createUnionType)
import           Data.Morpheus.Parsing.Internal.Internal (Parser)
import           Data.Morpheus.Parsing.Internal.Pattern  (fieldsDefinition, inputValueDefinition, optionalDirectives,
                                                          typDeclaration)
import           Data.Morpheus.Parsing.Internal.Terms    (keyword, operator, parseName, optDescription, pipeLiteral, sepByAnd, setOf)
import           Data.Morpheus.Types.Internal.Data       (DataField, DataFullType (..), Key, RawDataType (..))


-- Scalars : https://graphql.github.io/graphql-spec/June2018/#sec-Scalars
--
--  ScalarTypeDefinition:
--    Description(opt) scalar Name Directives(Const)(opt)
--
scalarTypeDefinition :: Maybe Text -> Parser (Text, DataFullType)
scalarTypeDefinition description =
  label "ScalarTypeDefinition" $ do
    name <- typDeclaration "scalar"
    -- TODO: handle directives
    _ <- optionalDirectives
    pure $ createScalarType name


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
objectTypeDefinition :: Maybe Text -> Parser (Text, RawDataType)
objectTypeDefinition description =
  label "ObjectTypeDefinition" $ do
    name <- typDeclaration "type"
    interfaces <- optionalImplementsInterfaces
    -- TODO: handle directives
    _directives <- optionalDirectives
    fields <- fieldsDefinition
    pure (name, Implements interfaces $ createType name fields)

optionalImplementsInterfaces :: Parser [Text]
optionalImplementsInterfaces = implements <|> pure []
  where
    implements = label "ImplementsInterfaces" $ keyword "implements" *> sepByAnd parseName

-- Interfaces: https://graphql.github.io/graphql-spec/June2018/#sec-Interfaces
--
--  InterfaceTypeDefinition
--    Description(opt) interface Name Directives(Const)(opt) FieldsDefinition(opt)
--
interfaceTypeDefinition :: Maybe Text -> Parser (Text, RawDataType)
interfaceTypeDefinition description =
  label "InterfaceTypeDefinition" $ do
    name <- typDeclaration "interface"
    -- TODO: handle directives
    _directives <- optionalDirectives
    fields <- fieldsDefinition
    pure (name, Interface $ createType name fields)


-- Unions : https://graphql.github.io/graphql-spec/June2018/#sec-Unions
--
--  UnionTypeDefinition:
--    Description(opt) union Name Directives(Const)(opt) UnionMemberTypes(opt)
--
--  UnionMemberTypes:
--    = |(opt) NamedType
--      UnionMemberTypes | NamedType
--
unionTypeDefinition :: Maybe Text ->  Parser (Text, DataFullType)
unionTypeDefinition description =
  label "UnionTypeDefinition" $ do
    name <- typDeclaration "union"
    -- TODO: handle directives
    _directives <- optionalDirectives
    createUnionType name <$> unionMemberTypes
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
enumTypeDefinition :: Maybe Text -> Parser (Text, DataFullType)
enumTypeDefinition description =
  label "EnumTypeDefinition" $ do
    name <- typDeclaration "enum"
    -- TODO: handle directives
    _directives <- optionalDirectives
    enumValuesDefinition <- setOf enumValueDefinition
    pure $ createEnumType name enumValuesDefinition
    where
        enumValueDefinition = do
            -- TODO: parse Description
            enumValueName <- parseName
            -- TODO: handle directives
            _directive <- optionalDirectives
            return enumValueName

-- Input Objects : https://graphql.github.io/graphql-spec/June2018/#sec-Input-Objects
--
--   InputObjectTypeDefinition
--     Description(opt) input Name  Directives(Const)(opt) InputFieldsDefinition(opt)
--
--   InputFieldsDefinition:
--     { InputValueDefinition(list) }
--
inputObjectTypeDefinition :: Maybe Text -> Parser (Text, DataFullType)
inputObjectTypeDefinition description =
  label "InputObjectTypeDefinition" $ do
    name <- typDeclaration "input"
    -- TODO: handle directives
    _directives <- optionalDirectives
    fields <- inputFieldsDefinition
    pure (name, InputObject $ createType name fields)
    where
      inputFieldsDefinition :: Parser [(Key, DataField)]
      inputFieldsDefinition = label "inputEntries" $ setOf inputValueDefinition


parseFinalDataType :: Maybe Text -> Parser (Text, DataFullType)
parseFinalDataType description = label "TypeDefinition" $ 
        inputObjectTypeDefinition description
    <|> unionTypeDefinition description
    <|> enumTypeDefinition description
    <|> scalarTypeDefinition description

parseDataType :: Parser (Text, RawDataType)
parseDataType = label "TypeDefinition" $ do 
    description <- optDescription
    types description
    where
      types description = interfaceTypeDefinition description <|> 
              objectTypeDefinition description <|> 
              finalDataT description
              where
                finalDataT description = do
                  (name, datatype) <- parseFinalDataType description
                  pure (name, FinalDataType datatype)

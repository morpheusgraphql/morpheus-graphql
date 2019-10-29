{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parsing.Document.TypeSystem
  ( parseDataType
  ) where

import           Data.Text                               (Text)
import           Text.Megaparsec                         (label, optional, sepBy1, (<|>))

-- MORPHEUS
import           Data.Morpheus.Parsing.Internal.Create   (createEnumType, createScalarType, createType, createUnionType)
import           Data.Morpheus.Parsing.Internal.Internal (Parser)
import           Data.Morpheus.Parsing.Internal.Pattern  (directive, fieldsDefinition, inputValueDefinition,
                                                          typDeclaration)
import           Data.Morpheus.Parsing.Internal.Terms    (keyword, operator, parseName, pipeLiteral, sepByAnd, setOf)
import           Data.Morpheus.Types.Internal.Data       (DataField, DataFullType (..), Key, RawDataType (..))


-- Scalars : https://graphql.github.io/graphql-spec/June2018/#sec-Scalars
--
--  ScalarTypeDefinition:
--    Description(opt) scalar Name Directives(Const)(opt)
--
scalarTypeDefinition :: Parser (Text, DataFullType)
scalarTypeDefinition =
  label "ScalarTypeDefinition" $ do
    name <- typDeclaration "scalar"
    -- TODO: handle directives
    _ <- optional directive
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
objectTypeDefinition :: Parser (Text, RawDataType)
objectTypeDefinition =
  label "ObjectTypeDefinition" $ do
    typeName <- typDeclaration "type"
    interfaces <- optionalImplementsInterfaces
    -- TODO: handle directives
    _directives <- optional directive
    fields <- fieldsDefinition
    pure (typeName, Implements interfaces $ createType typeName fields)

optionalImplementsInterfaces :: Parser [Text]
optionalImplementsInterfaces = implements <|> pure []
  where
    implements = label "ImplementsInterfaces" $ keyword "implements" *> sepByAnd parseName

-- Interfaces: https://graphql.github.io/graphql-spec/June2018/#sec-Interfaces
--
--  InterfaceTypeDefinition
--    Description(opt) interface Name Directives(Const)(opt) FieldsDefinition(opt)
--
interfaceTypeDefinition :: Parser (Text, RawDataType)
interfaceTypeDefinition =
  label "InterfaceTypeDefinition" $ do
    name <- typDeclaration "interface"
    -- TODO: handle directives
    _directives <- optional directive
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
unionTypeDefinition :: Parser (Text, DataFullType)
unionTypeDefinition =
  label "UnionTypeDefinition" $ do
    typeName <- typDeclaration "union"
    -- TODO: handle directives
    _directives <- optional directive
    createUnionType typeName <$> unionMemberTypes
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
enumTypeDefinition :: Parser (Text, DataFullType)
enumTypeDefinition =
  label "EnumTypeDefinition" $ do
    typeName <- typDeclaration "enum"
    -- TODO: handle directives
    _directives <- optional directive
    enumValuesDefinition <- setOf enumValueDefinition
    pure $ createEnumType typeName enumValuesDefinition
    where 
        enumValueDefinition = do 
            -- TODO: parse Description
            enumValueName <- parseName 
            -- TODO: handle directives
            _directive <- optional directive
            return enumValueName

-- Input Objects : https://graphql.github.io/graphql-spec/June2018/#sec-Input-Objects
--
--   InputObjectTypeDefinition
--     Description(opt) input Name  Directives(Const)(opt) InputFieldsDefinition(opt)
--
--   InputFieldsDefinition:
--     { InputValueDefinition(list) }
--
inputObjectTypeDefinition :: Parser (Text, DataFullType)
inputObjectTypeDefinition =
  label "InputObjectTypeDefinition" $ do
    name <- typDeclaration "input"
    -- TODO: handle directives
    _directives <- optional directive
    fields <- inputFieldsDefinition
    pure (name, InputObject $ createType name fields)
    where
      inputFieldsDefinition :: Parser [(Key, DataField)]
      inputFieldsDefinition = label "inputEntries" $ setOf inputValueDefinition


parseFinalDataType :: Parser (Text, DataFullType)
parseFinalDataType = label "dataType" $ inputObjectTypeDefinition
    <|> unionTypeDefinition
    <|> enumTypeDefinition
    <|> scalarTypeDefinition

parseDataType :: Parser (Text, RawDataType)
parseDataType = label "dataType" $ interfaceTypeDefinition <|> objectTypeDefinition <|> finalDataT
  where
    finalDataT = do
      (name, datatype) <- parseFinalDataType
      pure (name, FinalDataType datatype)

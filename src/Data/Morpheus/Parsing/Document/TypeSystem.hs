{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parsing.Document.TypeSystem
  ( parseDataType
  ) where

import           Data.Morpheus.Parsing.Internal.Create   (createEnumType, createScalarType, createType, createUnionType)
import           Data.Morpheus.Parsing.Internal.Internal (Parser)
import           Data.Morpheus.Parsing.Internal.Pattern  (directive, fieldsDefinition, inputValueDefinition)
import           Data.Morpheus.Parsing.Internal.Terms    (parseName, pipeLiteral, sepByAnd, setOf, spaceAndComments,
                                                          token)
import           Data.Morpheus.Parsing.Internal.Value    (parseDefaultValue)
import           Data.Morpheus.Types.Internal.Data       (DataField, DataFullType (..), Key, RawDataType (..))
import           Data.Text                               (Text)
import           Text.Megaparsec                         (label, optional, sepBy1, (<|>))
import           Text.Megaparsec.Char                    (char, space1, string)

-- Scalars : https://graphql.github.io/graphql-spec/June2018/#sec-Scalars
--
--  ScalarTypeDefinition:
--    Description(opt) scalar Name Directives(Const)(opt)
--
scalarTypeDefinition :: Parser (Text, DataFullType)
scalarTypeDefinition =
  label "scalar" $ do
    name <- typeDef "scalar"
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
  label "object" $ do
    typeName <- typeDef "type"
    interfaces <- maybeImplements
    typeData <- fieldsDefinition
    pure (typeName, Implements interfaces $ createType typeName typeData)

maybeImplements :: Parser [Text]
maybeImplements = implements <|> pure []
  where
    implements =
      label "implements" $ do
        _ <- string "implements"
        space1
        spaceAndComments
        sepByAnd token

-- Interfaces: https://graphql.github.io/graphql-spec/June2018/#sec-Interfaces
--
--  InterfaceTypeDefinition
--    Description(opt) interface Name Directives(Const)(opt) FieldsDefinition(opt)
--
interfaceTypeDefinition :: Parser (Text, RawDataType)
interfaceTypeDefinition =
  label "interface" $ do
    typeName <- typeDef "interface"
    typeData <- fieldsDefinition
    pure (typeName, Interface $ createType typeName typeData)


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
  label "union" $ do
    typeName <- typeDef "union"
    _ <- char '='
    spaceAndComments
    typeData <- unionsParser
    spaceAndComments
    pure $ createUnionType typeName typeData
  where
    unionsParser = token `sepBy1` pipeLiteral


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
enumValueDefinition :: Parser (Text, DataFullType)
enumValueDefinition =
  label "enum" $ do
    typeName <- typeDef "enum"
    typeData <- setOf ( token <* optional directive)
    pure $ createEnumType typeName typeData

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
  label "inputObject" $ do
    typeName <- typeDef "input"
    typeData <- inputFieldsDefinition
    pure (typeName, InputObject $ createType typeName typeData)
    where
      inputFieldsDefinition :: Parser [(Key, DataField)]
      inputFieldsDefinition = label "inputEntries" $ setOf inputValueDefinition

typeDef :: Text -> Parser Text
typeDef kind = do
  _ <- string kind
  space1
  parseName



parseFinalDataType :: Parser (Text, DataFullType)
parseFinalDataType = label "dataType" $ inputObjectTypeDefinition
    <|> unionTypeDefinition
    <|> enumValueDefinition
    <|> scalarTypeDefinition

parseDataType :: Parser (Text, RawDataType)
parseDataType = label "dataType" $ interfaceTypeDefinition <|> objectTypeDefinition <|> finalDataT
  where
    finalDataT = do
      (name, datatype) <- parseFinalDataType
      pure (name, FinalDataType datatype)

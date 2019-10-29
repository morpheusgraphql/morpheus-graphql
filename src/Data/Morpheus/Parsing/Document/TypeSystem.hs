{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parsing.Document.TypeSystem
  ( parseDataType
  ) where

import           Data.Morpheus.Parsing.Internal.Create   (createArgument, createEnumType, createField, createScalarType,
                                                          createType, createUnionType)
import           Data.Morpheus.Parsing.Internal.Internal (Parser)
import           Data.Morpheus.Parsing.Internal.Pattern  (directive, inputValueDefinition)
import           Data.Morpheus.Parsing.Internal.Terms    (parseAssignment, parseMaybeTuple, parseNonNull,
                                                          parseWrappedType, pipeLiteral, qualifier, sepByAnd, setOf,
                                                          spaceAndComments, token)
import           Data.Morpheus.Parsing.Internal.Value    (parseDefaultValue)
import           Data.Morpheus.Types.Internal.Data       (DataField, DataFullType (..), Key, RawDataType (..),
                                                          toHSWrappers)
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
    typeName <- typeDef "scalar"
    pure $ createScalarType typeName


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
    typeData <- outputObjectEntries
    pure (typeName, Implements interfaces $ createType typeName typeData)


entryWith :: Parser [(Text, DataField)] -> Parser (Key, DataField)
entryWith argsParser = label "entry" $ do
    ((fieldName, fieldArgs), (wrappers, fieldType)) <- parseAssignment fieldWithArgs parseWrappedType
    nonNull <- parseNonNull
    _ <- optional directive
    return (fieldName, createField fieldArgs fieldName (toHSWrappers $ nonNull ++ wrappers, fieldType))
    where
        fieldWithArgs =
          label "fieldWithArgs" $ do
            (name, _) <- qualifier
            args <- argsParser
            return (name, args)

outputObjectEntries :: Parser [(Key, DataField)]
outputObjectEntries = label "entries" $ setOf (entryWith (parseMaybeTuple dataArgument))



-- Interfaces: https://graphql.github.io/graphql-spec/June2018/#sec-Interfaces
--
--  InterfaceTypeDefinition
--    Description(opt) interface Name Directives(Const)(opt) FieldsDefinition(opt)
-- 
interfaceTypeDefinition :: Parser (Text, RawDataType)
interfaceTypeDefinition =
  label "interface" $ do
    typeName <- typeDef "interface"
    typeData <- outputObjectEntries
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
  token

maybeImplements :: Parser [Text]
maybeImplements = implements <|> pure []
  where
    implements =
      label "implements" $ do
        _ <- string "implements"
        space1
        spaceAndComments
        sepByAnd token



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

dataArgument :: Parser (Text, DataField)
dataArgument =
  label "Argument" $ do
    ((fieldName, _), (wrappers, fieldType)) <- parseAssignment qualifier parseWrappedType
    nonNull <- parseNonNull
    -- TODO: handle default value
    defaultValue <- parseDefaultValue
    pure $ createArgument fieldName (toHSWrappers $ nonNull ++ wrappers, fieldType)
